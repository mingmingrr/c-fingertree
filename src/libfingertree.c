#include "libfingertree.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <memory.h>

#ifndef NDEBUG
void showInt(FILE* file, void* num) {
	fprintf(file, "%zu", (size_t)num);
}
#endif

#ifndef NDEBUG
long refCounts[3] = { 0, 0, 0 };
long refCountGet(RefType type) { return refCounts[type]; }
#define refCountInc(type) ++refCounts[type]
#define refCountDec(type) --refCounts[type]
#else
#define refCountInc(type)
#define refCountDec(type)
#endif

// {{{ misc

Tree emptyTree = {
	.refs = 1,
	.type = EmptyT,
	.empty = NULL
};

static int Node_count(Node* node) {
	assert(node->size != 1);
	if(node->items[2] == NULL)
		return 2;
	else
		return 3;
}

static Tree* Tree_fromDigit(Digit* digit) {
	for(size_t i = 0; i < digit->count; ++i)
		Node_incRef(digit->items[i]);
	switch(digit->count) {
		case 0: return Empty_make();
		case 1: return Single_make(digit->items[0]);
		case 2: return Deep_make(digit->size, Digit_makeNS(1, digit->items),
			Empty_make(), Digit_makeNS(1, digit->items + 1));
		case 3: return Deep_make(digit->size, Digit_makeNS(2, digit->items),
			Empty_make(), Digit_makeNS(1, digit->items + 2));
		case 4: return Deep_make(digit->size, Digit_makeNS(2, digit->items),
			Empty_make(), Digit_makeNS(2, digit->items + 2));
		default: assert(false);
	}
}

// }}}

// {{{ incRef

Tree* Tree_incRef(Tree* tree) {
	assert(tree != NULL);
	++tree->refs;
	refCountInc(TreeR);
	return tree;
}

Digit* Digit_incRef(Digit* digit) {
	assert(digit != NULL);
	++digit->refs;
	refCountInc(DigitR);
	return digit;
}

Node* Node_incRef(Node* node) {
	if(node == NULL) return NULL;
	++node->refs;
	refCountInc(NodeR);
	return node;
}

static Node* Node_incRefM(Node* node) {
	if(node == NULL) return NULL;
	return Node_incRef(node);
}

IterCons* IterCons_incRef(IterCons* cons) {
	switch(cons->type) {
		case NodeI:  Node_incRef(cons->node);   break;
		case TreeI:  Tree_incRef(cons->tree);   break;
		case DigitI: Digit_incRef(cons->digit); break;
		default: assert(false);
	}
	return cons;
}

// }}}

// {{{ decRef

void Node_decRef(Node* node) {
	assert(node != NULL);
	refCountDec(NodeR);
	if(--node->refs == 0) {
		if(node->size > 1) {
			Node_decRef(node->items[0]);
			Node_decRef(node->items[1]);
			if(node->items[2] != NULL)
				Node_decRef(node->items[2]);
		}
		free(node);
	}
}

void* Node_decRefRet(Node* node, void* ret) {
	Node_decRef(node);
	return ret;
}

void Digit_decRef(Digit* digit) {
	assert(digit != NULL);
	refCountDec(DigitR);
	if(--digit->refs == 0) {
		switch(digit->count) {
			case 4: Node_decRef(digit->items[3]);
			case 3: Node_decRef(digit->items[2]);
			case 2: Node_decRef(digit->items[1]);
			case 1: Node_decRef(digit->items[0]);
				break;
			default: assert(false);
		}
		free(digit);
	}
}

void* Digit_decRefRet(Digit* digit, void* ret) {
	Digit_decRef(digit);
	return ret;
}

void Tree_decRef(Tree* tree) {
	assert(tree != NULL);
	refCountDec(TreeR);
	if(--tree->refs == 0) {
		switch(tree->type) {
			case EmptyT:
				break;
			case SingleT:
				Node_decRef(tree->single);
				break;
			case DeepT:
				Digit_decRef(tree->deep->left);
				Tree_decRef(tree->deep->middle);
				Digit_decRef(tree->deep->right);
				free(tree->deep);
				break;
			default: assert(false);
		}
		free(tree);
	}
}

void* Tree_decRefRet(Tree* tree, void* ret) {
	Tree_decRef(tree);
	return ret;
}

void IterCons_decRef(IterCons* cons) {
	switch(cons->type) {
		case NodeI: Node_decRef(cons->node); break;
		case TreeI: Tree_decRef(cons->tree); break;
		case DigitI: Digit_decRef(cons->digit); break;
		default: assert(false);
	}
}

void* IterCons_decRefRet(IterCons* cons, void* ret) {
	IterCons_decRef(cons);
	return ret;
}

// }}}

// {{{ alloc

Tree* Tree_alloc() {
	Tree* tree = malloc(sizeof(Tree));
	tree->refs = 1;
	refCountInc(TreeR);
	return tree;
}

Deep* Deep_alloc() {
	Deep* deep = malloc(sizeof(Deep));
	return deep;
}

Digit* Digit_alloc() {
	Digit* digit = malloc(sizeof(Digit));
	digit->refs = 1;
	refCountInc(DigitR);
	return digit;
}

Node* Node_alloc() {
	Node* node = malloc(sizeof(Node));
	node->refs = 1;
	refCountInc(NodeR);
	return node;
}

IterCons* IterCons_alloc() {
	return malloc(sizeof(IterCons));
}

Iter* Iter_alloc() {
	return malloc(sizeof(Iter));
}

// }}}

// {{{ make / replace

Tree* Empty_make() {
	++emptyTree.refs;
	refCountInc(TreeR);
	return &emptyTree;
}

Tree* Single_make(Node* node) {
	Tree* tree = Tree_alloc();
	tree->type = SingleT;
	tree->single = node;
	return tree;
}

Tree* Deep_make(size_t size, Digit* left, Tree* middle, Digit* right) {
	Deep* deep = Deep_alloc();
	deep->size = size;
	deep->left = left;
	deep->middle = middle;
	deep->right = right;
	Tree* tree = Tree_alloc();
	tree->type = DeepT;
	tree->deep = deep;
	return tree;
}

Tree* Deep_makeS(Digit* left, Tree* middle, Digit* right) {
	size_t size = left->size + Tree_size(middle) + right->size;
	return Deep_make(size, left, middle, right);
}


Digit* Digit_make(
	size_t size, size_t count,
	Node* n0, Node* n1, Node* n2, Node* n3
) {
	assert(1 <= count && count <= 4);
	Digit* digit = Digit_alloc();
	digit->size = size;
	digit->count = count;
	digit->items[0] = n0;
	digit->items[1] = n1; assert((count < 2) == (n1 == NULL));
	digit->items[2] = n2; assert((count < 3) == (n2 == NULL));
	digit->items[3] = n3; assert((count < 4) == (n3 == NULL));
	return digit;
}

Digit* Digit_makeN(size_t size, size_t count, Node** nodes) {
	switch(count) {
		case 1: return Digit_make(size,
			count, nodes[0], NULL, NULL, NULL);
		case 2: return Digit_make(size,
			count, nodes[0], nodes[1], NULL, NULL);
		case 3: return Digit_make(size,
			count, nodes[0], nodes[1], nodes[2], NULL);
		case 4: return Digit_make(size,
			count, nodes[0], nodes[1], nodes[2], nodes[3]);
		default: assert(false);
	}
}

Digit* Digit_makeNS(size_t count, Node** nodes) {
	assert(nodes[0] != NULL);
	size_t size = nodes[0]->size;
	switch(count) {
		case 4: assert(nodes[3] != NULL); size += nodes[3]->size;
		case 3: assert(nodes[2] != NULL); size += nodes[2]->size;
		case 2: assert(nodes[1] != NULL); size += nodes[1]->size;
		case 1: break;
		default: assert(false);
	}
	return Digit_makeN(size, count, nodes);
}

Digit* Digit_fromNode(Node* node) {
	return Digit_make(node->size, Node_count(node),
		Node_incRef(node->items[0]),
		Node_incRef(node->items[1]),
		Node_incRefM(node->items[2]),
		NULL);
}

Node* Node_make(size_t size, Node* n0, Node* n1, Node* n2) {
	Node* node = Node_alloc();
	node->size = size;
	node->items[0] = n0; assert(node->size == 1 || n0 != NULL);
	node->items[1] = n1; assert(n1 != NULL || size == 1);
	node->items[2] = n2;
	return node;
}

Node* Node_makeS(Node* n0, Node* n1, Node* n2) {
	size_t size = n0->size + n1->size + (n2 == NULL ? 0 : n2->size);
	return Node_make(size, n0, n1, n2);
}

Node* Node_makeNS(size_t count, Node** nodes) {
	switch(count) {
		case 2: return Node_make(
			nodes[0]->size + nodes[1]->size,
			nodes[0], nodes[1], NULL);
		case 3: return Node_make(
			nodes[0]->size + nodes[1]->size + nodes[2]->size,
			nodes[0], nodes[1], nodes[2]);
		default: assert(false);
	}
}

Node* Node_make1(void* item) {
	return Node_make(1, item, NULL, NULL);
}

IterCons* IterCons_make(IterType type, void* item, IterCons* next) {
	IterCons* cons = IterCons_alloc();
	cons->type = type;
	cons->index = 0;
	cons->tree = item;
	cons->next = next;
	return IterCons_incRef(cons);
}

Iter* Iter_replace(Iter* iter, IterType type, void* item) {
	IterCons_decRef(iter->stack);
	iter->stack->type = type;
	iter->stack->index = 0;
	iter->stack->tree = item;
	IterCons_incRef(iter->stack);
	return iter;
}

Iter* Iter_make(IterCons* stack) {
	Iter* iter = Iter_alloc();
	iter->stack = stack;
	return iter;
}

// }}}

// {{{ print

#ifndef NDEBUG

static void Indent_fprint(FILE* file, int indent) {
	// fprintf(file, "%d ", indent);
	for(int i = 0; i < indent; ++i)
		fprintf(file, "  ");
}

void Node_fprint(
	FILE* file,
	Node* node,
	int indent,
	void(*show)(FILE*,void*)
) {
	Indent_fprint(file, indent);
	if(node->size == 1) {
		fprintf(file, "Element(%zu) ", node->refs);
		show(file, node->items[0]);
		fprintf(file, "\n");
	} else {
		fprintf(file, "Node[%zu](%zu)\n", node->size, node->refs);
		Node_fprint(file, (Node*)node->items[0], indent + 1, show);
		Node_fprint(file, (Node*)node->items[1], indent + 1, show);
		if(node->items[2] != NULL)
			Node_fprint(file, (Node*)node->items[2], indent + 1, show);
	}
}

void Node_print(Node* node) {
	Node_fprint(stdout, node, 0, showInt);
}

void Digit_fprint(
	FILE* file,
	Digit* digit,
	int indent,
	void(*show)(FILE*,void*)
) {
	Indent_fprint(file, indent);
	fprintf(file, "Digit[%zu](%zu)\n", digit->size, digit->refs);
	for(int i = 0; i < digit->count; ++i)
		Node_fprint(file, digit->items[i], indent + 1, show);
}

void Digit_print(Digit* digit) {
	Digit_fprint(stdout, digit, 0, showInt);
}

void Tree_fprint(
	FILE* file,
	Tree* tree,
	int indent,
	void(*show)(FILE*,void*)
) {
	Indent_fprint(file, indent);
	switch(tree->type) {
		case EmptyT:
			fprintf(file, "Empty(%zu)\n", tree->refs);
			break;
		case SingleT:
			fprintf(file, "Single(%zu)\n", tree->refs);
			Node_fprint(file, tree->single, indent + 1, show);
			break;
		case DeepT:
			fprintf(file, "Deep[%zu](%zu)\n",
				tree->deep->size, tree->refs);
			Digit_fprint(file, tree->deep->left, indent + 1, show);
			Tree_fprint(file, tree->deep->middle, indent + 1, show);
			Digit_fprint(file, tree->deep->right, indent + 1, show);
			break;
		default: assert(false);
	}
}

void Tree_print(Tree* tree) {
	Tree_fprint(stdout, tree, 0, showInt);
}

static void IterCons_fprint(
	FILE* file,
	IterCons* cons,
	int indent,
	bool showNode,
	void(*show)(FILE*,void*)
) {
	Indent_fprint(file, indent);
	if(cons == NULL) {
		fprintf(file, "Cons (nil)\n");
		return;
	}
	fprintf(file, "Cons %u ", cons->index);
	switch(cons->type) {
		case TreeI:
			fprintf(file, "Tree\n");
			Indent_fprint(file, indent + 1);
			fprintf(file, "%p\n", cons->tree);
			if(showNode) Tree_fprint(file, cons->tree, indent + 1, show);
			break;
		case DigitI:
			fprintf(file, "Digit\n");
			if(showNode) Digit_fprint(file, cons->digit, indent + 1, show);
			break;
		case NodeI:
			fprintf(file, "Node\n");
			if(showNode) Node_fprint(file, cons->node, indent + 1, show);
			break;
		default: assert(false);
	}
	IterCons_fprint(file, cons->next, indent + 1, showNode, show);
}

void Iter_fprint(
	FILE* file,
	Iter* iter,
	int indent,
	bool showNode,
	void(*show)(FILE*,void*)
) {
	Indent_fprint(file, indent);
	fprintf(file, "Iter\n");
	IterCons_fprint(file, iter->stack, indent + 1, showNode, show);
}

void Iter_print(Iter* iter, bool showNode) {
	Iter_fprint(stdout, iter, 0, showNode, showInt);
}

#endif

// }}}

// {{{ bool, len

bool Tree_empty(Tree* tree) {
	return tree->type != EmptyT;
}

size_t Tree_size(Tree* tree) {
	switch(tree->type) {
		case EmptyT:  return 0;
		case SingleT: return tree->single->size;
		case DeepT:   return tree->deep->size;
		default:      assert(false);
	}
}

// }}}

// {{{ pull

static View Tree_viewLeftN(Tree* tree);

Tree* Tree_pullLeft(Tree* middle, Digit* right) {
	View view = Tree_viewLeftN(middle);
	if(view.tree == NULL)
		return Tree_fromDigit(right);
	Tree* tail = Deep_make(Tree_size(middle) + right->size,
		Digit_fromNode(view.item), view.tree,
		Digit_incRef(right));
	Node_decRef(view.item);
	return tail;
}

static View Tree_viewRightN(Tree* tree);

Tree* Tree_pullRight(Tree* middle, Digit* left) {
	View view = Tree_viewRightN(middle);
	if(view.tree == NULL)
		return Tree_fromDigit(left);
	Tree* init = Deep_make(Tree_size(middle) + left->size,
		Digit_incRef(left), view.tree,
		Digit_fromNode(view.item));
	Node_decRef(view.item);
	return init;
}

// }}}

// {{{ appendLeft

static Digit* Digit_appendLeftN(Digit* digit, Node* node) {
	assert(digit->count < 4);
	switch(digit->count) {
		case 3: return Digit_make(digit->size + node->size, 4, node,
			Node_incRef(digit->items[0]),
			Node_incRef(digit->items[1]),
			Node_incRef(digit->items[2]));
		case 2: return Digit_make(digit->size + node->size, 3, node,
			Node_incRef(digit->items[0]),
			Node_incRef(digit->items[1]), NULL);
		case 1: return Digit_make(digit->size + node->size, 2, node,
			Node_incRef(digit->items[0]), NULL, NULL);
		default: assert(false);
	}
}

Tree* Tree_appendLeftN(Tree* tree, Node* node) {
	switch(tree->type) {
		case EmptyT:
			return Single_make(node);
		case SingleT:
			return Deep_make(tree->single->size + node->size,
				Digit_make(node->size, 1, node, NULL, NULL, NULL),
				Empty_make(),
				Digit_make(tree->single->size, 1,
					Node_incRef(tree->single), NULL, NULL, NULL));
		case DeepT:
			if(tree->deep->left->count < 4)
				return Deep_make(tree->deep->size + node->size,
					Digit_appendLeftN(tree->deep->left, node),
					Tree_incRef(tree->deep->middle),
					Digit_incRef(tree->deep->right));
			return Deep_make(tree->deep->size + node->size,
				Digit_make(tree->deep->left->items[0]->size + node->size, 2,
					node, Node_incRef(tree->deep->left->items[0]), NULL, NULL),
				Tree_appendLeftN(tree->deep->middle, Node_make(
					tree->deep->left->size - tree->deep->left->items[0]->size,
					Node_incRef(tree->deep->left->items[1]),
					Node_incRef(tree->deep->left->items[2]),
					Node_incRef(tree->deep->left->items[3]))),
				Digit_incRef(tree->deep->right));
		default: assert(false);
	}
}

Tree* Tree_appendLeft(Tree* tree, void* item) {
	return Tree_appendLeftN(tree, Node_make1(item));
}

// }}}

// {{{ appendRight

static Digit* Digit_appendRightN(Digit* digit, Node* node) {
	assert(digit->count < 4);
	switch(digit->count) {
		case 3: return Digit_make(digit->size + node->size, 4,
			Node_incRef(digit->items[0]),
			Node_incRef(digit->items[1]),
			Node_incRef(digit->items[2]),
			node);
		case 2: return Digit_make(digit->size + node->size, 3,
			Node_incRef(digit->items[0]),
			Node_incRef(digit->items[1]),
			node, NULL);
		case 1: return Digit_make(digit->size + node->size, 2,
			Node_incRef(digit->items[0]),
			node, NULL, NULL);
		default: assert(false);
	}
}

static Tree* Tree_appendRightN(Tree* tree, Node* node) {
	switch(tree->type) {
		case EmptyT:
			return Single_make(node);
		case SingleT:
			return Deep_make(tree->single->size + node->size,
				Digit_make(tree->single->size, 1,
					Node_incRef(tree->single), NULL, NULL, NULL),
				Empty_make(),
				Digit_make(node->size, 1, node, NULL, NULL, NULL));
		case DeepT:
			if(tree->deep->right->count < 4)
				return Deep_make(tree->deep->size + node->size,
					Digit_incRef(tree->deep->left),
					Tree_incRef(tree->deep->middle),
					Digit_appendRightN(tree->deep->right, node));
			return Deep_make(tree->deep->size + node->size,
				Digit_incRef(tree->deep->left),
				Tree_appendRightN(tree->deep->middle, Node_make(
					tree->deep->right->size - tree->deep->right->items[3]->size ,
					Node_incRef(tree->deep->right->items[0]),
					Node_incRef(tree->deep->right->items[1]),
					Node_incRef(tree->deep->right->items[2]))),
				Digit_make(tree->deep->right->items[3]->size + node->size,
					2, Node_incRef(tree->deep->right->items[3]),
					node, NULL, NULL));
		default: assert(false);
	}
}

Tree* Tree_appendRight(Tree* tree, void* item) {
	return Tree_appendRightN(tree, Node_make1(item));
}

// }}}

// {{{ viewLeft

static View Tree_viewLeftN(Tree* tree) {
	assert(tree != NULL);
	switch(tree->type) {
		case EmptyT: return (View){NULL, NULL};
		case SingleT: return (View){Node_incRef(tree->single), Empty_make()};
		case DeepT: {
			Digit* left = tree->deep->left;
			Node* head = Node_incRef(left->items[0]);
			if(left->count == 1) return (View){ head,
				Tree_pullLeft(tree->deep->middle, tree->deep->right) };
			for(size_t i = 1; i < left->count; ++i)
				Node_incRef(left->items[i]);
			Tree* tail = Deep_make(tree->deep->size - head->size,
				Digit_make(left->size - head->size, left->count - 1,
					left->items[1], left->items[2], left->items[3], NULL),
				Tree_incRef(tree->deep->middle),
				Digit_incRef(tree->deep->right));
			return (View){head, tail};
		}
		default: assert(false);
	}
}

View Tree_viewLeft(Tree* tree) {
	View view = Tree_viewLeftN(tree);
	if(view.tree != NULL) {
		Node* node = view.item;
		assert(node->size == 1);
		view.item = node->value;
		Node_decRef(node);
	}
	return view;
}

View* Tree_viewLeftPtr(Tree* tree) {
	View* view = malloc(sizeof(View));
	*view = Tree_viewLeft(tree);
	return view;
}

// }}}

// {{{ viewRight

static View Tree_viewRightN(Tree* tree) {
	assert(tree != NULL);
	switch(tree->type) {
		case EmptyT: return (View){NULL, NULL};
		case SingleT: return (View){Node_incRef(tree->single), Empty_make()};
		case DeepT: {
			Digit* right = tree->deep->right;
			Node* last = Node_incRef(right->items[right->count-1]);
			if(right->count == 1) return (View){ last,
				Tree_pullRight(tree->deep->middle, tree->deep->left) };
			for(size_t i = 0; i < right->count - 1; ++i)
				Node_incRef(right->items[i]);
			Tree* init = Deep_make(tree->deep->size - last->size,
				Digit_incRef(tree->deep->left),
				Tree_incRef(tree->deep->middle),
				Digit_makeN(right->size - last->size,
					right->count - 1, right->items));
			return (View){last, init};
		}
		default: assert(false);
	}
}

View Tree_viewRight(Tree* tree) {
	View view = Tree_viewRightN(tree);
	if(view.tree != NULL) {
		Node* node = view.item;
		assert(node->size == 1);
		view.item = node->value;
		Node_decRef(node);
	}
	return view;
}

View* Tree_viewRightPtr(Tree* tree) {
	View* view = malloc(sizeof(View));
	*view = Tree_viewRight(tree);
	return view;
}

// }}}

// {{{ fromArray

static Tree* Tree_fromNodes(size_t size, size_t count, Node** nodes) {
	if(count == 0) return Empty_make();
	if(count == 1) return Single_make(nodes[0]);
	if(count <= 8) return Deep_make(size,
		Digit_makeNS(count >> 1, nodes), Empty_make(),
		Digit_makeNS(count - (count >> 1), nodes + (count >> 1)));
	size_t countN = (count + 2) / 3 - 2;
	Node** nodesN = malloc(countN * sizeof(Node*));
	for(size_t i = 2, j = 3; i < countN; ++i, j += 3)
		nodesN[i-2] = Node_makeS(nodes[j], nodes[j+1], nodes[j+2]);
	switch(count % 3) {
		case 0:
			assert(countN >= 1);
			if(countN >= 2)
				nodesN[countN-2] = Node_makeS(
					nodes[count-9], nodes[count-8], nodes[count-7]);
			nodesN[countN-1] = Node_makeS(
				nodes[count-6], nodes[count-5], nodes[count-4]);
			break;
		case 1:
			assert(countN >= 2);
			nodesN[countN-2] = Node_makeS(
				nodes[count-7], nodes[count-6], NULL);
			nodesN[countN-1] = Node_makeS(
				nodes[count-5], nodes[count-4], NULL);
			break;
		case 2:
			assert(countN >= 2);
			nodesN[countN-2] = Node_makeS(
				nodes[count-8], nodes[count-7], nodes[count-6]);
			nodesN[countN-1] = Node_makeS(
				nodes[count-5], nodes[count-4], NULL);
			break;
		default: assert(false);
	}
	Digit* left = Digit_make(
		nodes[0]->size + nodes[1]->size + nodes[2]->size,
		3, nodes[0], nodes[1], nodes[2], NULL);
	Digit* right = Digit_make(
		nodes[count-3]->size + nodes[count-2]->size + nodes[count-3]->size,
		3, nodes[count-3], nodes[count-2], nodes[count-1], NULL);
	Tree* tree = Deep_make(size, left,
		Tree_fromNodes(count - 6, countN, nodesN), right);
	free(nodesN);
	return tree;
}

Tree* Tree_fromArray(size_t size, void** items) {
	Node** nodes = malloc(size * sizeof(Node*));
	for(size_t i = 0; i < size; ++i)
		nodes[i] = Node_make1(items[i]);
	Tree* tree = Tree_fromNodes(size, size, nodes);
	free(nodes);
	return tree;
}

// }}}

// {{{ iteration

static Iter* Iter_pushStack(Iter* iter, IterType type, void* item) {
	iter->stack = IterCons_make(type, item, iter->stack);
	return iter;
}

static Iter* Iter_popStack(Iter* iter) {
	assert(iter->stack != NULL);
	IterCons* cons = iter->stack;
	IterCons_decRef(cons);
	iter->stack = cons->next;
	free(cons);
	return iter;
}

bool Iter_empty(Iter* iter) {
	return iter->stack == NULL;
}

static Iter* Iter_advance(Iter* iter) {
	if(iter->stack == NULL) return NULL;
	switch(iter->stack->type) {
		case TreeI:
			switch(iter->stack->tree->type) {
				case EmptyT:
					assert(iter->stack->index == 0);
					return Iter_advance(Iter_popStack(iter));
				case SingleT:
					assert(iter->stack->index == 0);
					return Iter_advance(Iter_replace(iter, NodeI,
						iter->stack->tree->single));
				case DeepT:
					switch(iter->stack->index++) {
						case 0:
							return Iter_advance(Iter_pushStack(iter, DigitI,
								iter->stack->tree->deep->left));
						case 1:
							return Iter_advance(Iter_pushStack(iter, TreeI,
								iter->stack->tree->deep->middle));
						case 2:
							return Iter_advance(Iter_replace(iter, DigitI,
								iter->stack->tree->deep->right));
						default: assert(false);
					}
				default: assert(false);
			};
		case DigitI:
			assert(iter->stack->index <= 4);
			if(iter->stack->index == iter->stack->digit->count)
				return Iter_advance(Iter_popStack(iter));
			return Iter_advance(Iter_pushStack(iter, NodeI,
				iter->stack->digit->items[iter->stack->index++]));
		case NodeI:
			if(iter->stack->node->size == 1) {
				assert(iter->stack->index == 0);
				return iter;
			}
			assert(iter->stack->index <= 3);
			if(iter->stack->index == Node_count(iter->stack->node))
				return Iter_advance(Iter_popStack(iter));
			return Iter_advance(Iter_pushStack(iter, NodeI,
				iter->stack->node->items[iter->stack->index++]));
		default: assert(false);
	}
}

void* Iter_next(Iter* iter) {
	assert(!Iter_empty(iter));
	Node* node = iter->stack->node;
	assert(node->size == 1);
	Iter_popStack(iter);
	Iter_advance(iter);
	return node->value;
}

Iter* Iter_fromTree(Tree* tree) {
	switch(tree->type) {
		case EmptyT: return Iter_make(NULL);
		default: {
			Iter* iter = Iter_make(IterCons_make(TreeI, tree, NULL));
			Iter_advance(iter);
			return iter;
		}
	}
}

// }}}

// {{{ toArray

static void** Node_toArrayItems(Node* node, void** items) {
	assert(node != NULL);
	if(node->size == 1) {
		*items = node->value;
		return ++items;
	}
	items = Node_toArrayItems(node->items[0], items);
	items = Node_toArrayItems(node->items[1], items);
	if(node->items[2] != NULL)
		items = Node_toArrayItems(node->items[2], items);
	return items;
}

static void** Digit_toArrayItems(Digit* digit, void** items) {
	assert(digit != NULL);
	for(size_t i = 0; i < digit->count; ++i)
		items = Node_toArrayItems(digit->items[i], items);
	return items;
}

static void** Tree_toArrayItems(Tree* tree, void** items) {
	assert(tree != NULL);
	switch(tree->type) {
		case EmptyT: return items;
		case SingleT: return Node_toArrayItems(tree->single, items);
		case DeepT:
			items = Digit_toArrayItems(tree->deep->left, items);
			items = Tree_toArrayItems(tree->deep->middle, items);
			return Digit_toArrayItems(tree->deep->right, items);
		default: assert(false);
	}
}

void** Tree_toArray(Tree* tree) {
	size_t size = Tree_size(tree);
	void** items = malloc(size * sizeof(void*));
	void** end = Tree_toArrayItems(tree, items);
	assert(items + size == end);
	return items;
}

// }}}

// {{{ extend

Tree* Tree_extend(Tree* xs, Tree* ys) {
	switch(xs->type) {
		case EmptyT: return Tree_incRef(ys);
		case SingleT: return Tree_appendLeftN(ys, Node_incRef(xs->single));
		case DeepT: switch(ys->type) {
			case EmptyT: return Tree_incRef(xs);
			case SingleT: return Tree_appendRightN(xs, Node_incRef(ys->single));
			case DeepT: {
				size_t size = xs->deep->size + ys->deep->size;
				Node* mid[8]; size_t count = 0;
				for(; count < xs->deep->right->count; ++count)
					mid[count] = Node_incRef(xs->deep->right->items[count]);
				for(size_t i = 0; i < ys->deep->left->count; ++i, ++count)
					mid[count] = Node_incRef(ys->deep->left->items[i]);
				Tree* right = Tree_incRef(ys->deep->middle);
				switch(count) {
					case 8: right = Tree_decRefRet(right, Tree_appendLeftN(right,
						Node_makeS(mid[5], mid[6], mid[7])));
					case 5: right = Tree_decRefRet(right, Tree_appendLeftN(right,
						Node_makeS(mid[2], mid[3], mid[4])));
					case 2: right = Tree_decRefRet(right, Tree_appendLeftN(right,
						Node_makeS(mid[0], mid[1], NULL)));
					break;
					case 6: right = Tree_decRefRet(right, Tree_appendLeftN(right,
						Node_makeS(mid[3], mid[4], mid[5])));
					case 3: right = Tree_decRefRet(right, Tree_appendLeftN(right,
						Node_makeS(mid[0], mid[1], mid[2])));
					break;
					case 7: right = Tree_decRefRet(right, Tree_appendLeftN(right,
						Node_makeS(mid[4], mid[5], mid[6])));
					case 4:
						right = Tree_decRefRet(right, Tree_appendLeftN(right,
							Node_makeS(mid[2], mid[3], NULL)));
						right = Tree_decRefRet(right, Tree_appendLeftN(right,
							Node_makeS(mid[0], mid[1], NULL)));
					break;
					default: assert(false);
				}
				return Tree_decRefRet(right, Deep_make(size,
					Digit_incRef(xs->deep->left),
					Tree_extend(xs->deep->middle, right),
					Digit_incRef(ys->deep->right)));
			}
			default: assert(false);
		}
		default: assert(false);
	}
}

// }}}

// {{{ index

static void* Node_index(Node* node, size_t index) {
	assert(node != NULL);
	assert(index < node->size);
	if(node->size == 1)
		return node->value;
	if(index < node->items[0]->size)
		return Node_index(node->items[0], index);
	index -= node->items[0]->size;
	if(index < node->items[1]->size)
		return Node_index(node->items[1], index);
	index -= node->items[1]->size;
	return Node_index(node->items[2], index);
}

static void* Digit_index(Digit* digit, size_t index) {
	assert(index < digit->size);
	for(size_t i = 0; i < digit->count; ++i)
		if(index < digit->items[i]->size)
			return Node_index(digit->items[i], index);
		else
			index -= digit->items[i]->size;
	assert(false);
}

void* Tree_index(Tree* tree, size_t index) {
	assert(index < Tree_size(tree));
	switch(tree->type) {
		case SingleT: return Node_index(tree->single, index);
		case DeepT:
			if(index < tree->deep->left->size)
				return Digit_index(tree->deep->left, index);
			index -= tree->deep->left->size;
			if(index < Tree_size(tree->deep->middle))
				return Tree_index(tree->deep->middle, index);
			index -= Tree_size(tree->deep->middle);
			return Digit_index(tree->deep->right, index);
		default: assert(false);
	}
}

// }}}

// {{{ update

static Node* Node_update(Node* node, size_t index, void* value) {
	assert(index < node->size);
	if(node->size == 1)
		return Node_make1(value);
	if(index < node->items[0]->size)
		return Node_make(node->size,
			Node_update(node->items[0], index, value),
			Node_incRef(node->items[1]),
			Node_incRefM(node->items[2]));
	index -= node->items[0]->size;
	if(index < node->items[1]->size)
		return Node_make(node->size,
			Node_incRef(node->items[0]),
			Node_update(node->items[1], index, value),
			Node_incRefM(node->items[2]));
	index -= node->items[1]->size;
	return Node_make(node->size,
		Node_incRef(node->items[0]),
		Node_incRefM(node->items[1]),
		Node_update(node->items[2], index, value));
}

static Digit* Digit_update(Digit* digit, size_t index, void* value) {
	assert(index < digit->size);
	Node* items[4] = {NULL, NULL, NULL, NULL};
	for(size_t i = 0; i < digit->count; ++i)
		if(index < digit->items[i]->size) {
			items[i] = Node_update(digit->items[i], index, value);
			for(size_t j = i + 1; j < digit->count; ++j)
				items[j] = Node_incRef(digit->items[j]);
			return Digit_make(digit->size, digit->count,
				items[0], items[1], items[2], items[3]);
		} else {
			items[i] = Node_incRef(digit->items[i]);
			index -= digit->items[i]->size;
		}
	assert(false);
}

Tree* Tree_update(Tree* tree, size_t index, void* value) {
	assert(index < Tree_size(tree));
	switch(tree->type) {
		case EmptyT: return Empty_make();
		case SingleT: return Single_make(Node_update(tree->single, index, value));
		case DeepT:
			if(index < tree->deep->left->size)
				return Deep_make(tree->deep->size,
					Digit_update(tree->deep->left, index, value),
					Tree_incRef(tree->deep->middle),
					Digit_incRef(tree->deep->right));
			index -= tree->deep->left->size;
			if(index < Tree_size(tree->deep->middle))
				return Deep_make(tree->deep->size,
					Digit_incRef(tree->deep->left),
					Tree_update(tree->deep->middle, index, value),
					Digit_incRef(tree->deep->right));
			index -= Tree_size(tree->deep->middle);
			return Deep_make(tree->deep->size,
				Digit_incRef(tree->deep->left),
				Tree_incRef(tree->deep->middle),
				Digit_update(tree->deep->right, index, value));
		default: assert(false);
	}
}

// }}}

// {{{ splitAt

static Split Tree_splitAtN(Tree* tree, size_t index);

static Split Deep_splitLeftN(Deep* deep, size_t index) {
	size_t size, dsize = 0;
	Node* prefix[4] = { NULL, NULL, NULL, NULL };
	for(size_t i = 0; i < deep->left->count; ++i)
		if(index >= (size = deep->left->items[i]->size)) {
			prefix[i] = Node_incRef(deep->left->items[i]);
			index -= size; dsize += size;
		} else if(i + 1 == deep->left->count) {
			return (Split){
				Tree_fromNodes(dsize, i, prefix),
				Node_incRef(deep->left->items[i]),
				Tree_pullLeft(deep->middle, deep->right) };
		} else {
			Node* suffix[4] = { NULL, NULL, NULL, NULL };
			for(size_t j = i + 1, k = 0; j < deep->left->count; ++j, ++k)
				suffix[k] = Node_incRef(deep->left->items[j]);
			return (Split){
				Tree_fromNodes(dsize, i, prefix),
				Node_incRef(deep->left->items[i]),
				Deep_make(deep->size - dsize - size,
					Digit_makeN(deep->left->size - dsize - size,
						deep->left->count - i - 1, suffix),
					Tree_incRef(deep->middle),
					Digit_incRef(deep->right)) };
		}
}

static Split Deep_splitRightN(Deep* deep, size_t index) {
	size_t size, dsize = 0;
	Node* prefix[4] = { NULL, NULL, NULL, NULL };
	for(size_t i = 0; i < deep->right->count; ++i)
		if(index >= (size = deep->right->items[i]->size)) {
			prefix[i] = Node_incRef(deep->right->items[i]);
			index -= size; dsize += size;
		} else if(i == 0) {
			for(size_t j = 1; j < deep->right->count; ++j)
				prefix[j-1] = Node_incRef(deep->right->items[j]);
			return (Split){
				Tree_pullRight(deep->middle, deep->left),
				Node_incRef(deep->right->items[0]),
				Tree_fromNodes(deep->right->size - size,
					deep->right->count - 1, prefix) };
		} else {
			Node* suffix[4] = { NULL, NULL, NULL, NULL };
			for(size_t j = i + 1, k = 0; j < deep->right->count; ++j, ++k)
				suffix[k] = Node_incRef(deep->right->items[j]);
			return (Split){
				Deep_make(deep->size - deep->right->size + dsize,
					Digit_incRef(deep->left),
					Tree_incRef(deep->middle),
					Digit_makeN(dsize, i, prefix)),
				Node_incRef(deep->right->items[i]),
				Tree_fromNodes(deep->size - dsize - size,
					deep->right->count - i - 1, suffix) };
		}
}

static Split Deep_splitMiddleN(Deep* deep, size_t index) {
	Split split = Tree_splitAtN(deep->middle, index);
	if(split.node->size == 1) {
		Tree* left = Tree_decRefRet(split.left,
			Tree_pullRight(split.left, deep->left));
		Tree* right = Tree_decRefRet(split.right,
			Tree_pullRight(split.right, deep->right));
		return (Split){ left, split.node, right };
	}
	index -= Tree_size(split.left);
	size_t size, presize = 0;
	if(index < (size = split.node->items[0]->size)) {
		Tree* left = Tree_decRefRet(split.left,
			Tree_pullRight(split.left, deep->left));
		Node* middle = Node_incRef(split.node->items[0]);
		Tree* right = Deep_makeS(
			Digit_make(split.node->size - size,
				Node_count(split.node) - 1,
				Node_incRef(split.node->items[1]),
				Node_incRefM(split.node->items[2]), NULL, NULL),
			split.right, Digit_incRef(deep->right));
		Node_decRef(split.node);
		return (Split){ left, middle, right };
	}
	index -= size; presize += size;
	if(index < (size = split.node->items[1]->size)) {
		Tree* left = Deep_makeS(
			Digit_incRef(deep->left), split.left,
			Digit_make(split.node->items[0]->size, 1,
				Node_incRef(split.node->items[0]), NULL, NULL, NULL));
		Node* middle = Node_incRef(split.node->items[1]);
		Tree* right = split.node->items[2] == NULL
			? Tree_decRefRet(split.right,
				Tree_pullLeft(split.right, deep->right))
			: Deep_makeS(Digit_make(split.node->items[2]->size, 1,
					Node_incRefM(split.node->items[2]), NULL, NULL, NULL),
				split.right, Digit_incRef(deep->right));
		Node_decRef(split.node);
		return (Split){ left, middle, right };
	}
	index -= size; presize += size;
	assert(split.node->items[2] != NULL); {
		size = split.node->items[2]->size;
		Tree* left = Deep_makeS(
			Digit_incRef(deep->left), split.left,
			Digit_make(split.node->size - size, 2,
				Node_incRef(split.node->items[0]),
				Node_incRef(split.node->items[1]), NULL, NULL));
		Node* middle = Node_incRef(split.node->items[2]);
		Tree* right = Tree_decRefRet(split.right,
			Tree_pullLeft(split.right, deep->right));
		Node_decRef(split.node);
		return (Split){ left, middle, right };
	}
}

static Split Tree_splitAtN(Tree* tree, size_t index) {
	assert(index < Tree_size(tree));
	switch(tree->type) {
		case SingleT: return (Split){ Empty_make(),
			Node_incRef(tree->single), Empty_make() };
		case DeepT:
			if(index < tree->deep->left->size)
				return Deep_splitLeftN(tree->deep, index);
			index -= tree->deep->left->size;
			if(index < Tree_size(tree->deep->middle))
				return Deep_splitMiddleN(tree->deep, index);
			index -= Tree_size(tree->deep->middle);
			return Deep_splitRightN(tree->deep, index);
		default: assert(false);
	}
}

Split Tree_splitAt(Tree* tree, size_t index) {
	Split split = Tree_splitAtN(tree, index);
	assert(split.node->size == 1);
	split.item = Node_decRefRet(split.node, split.node->items[0]);
	return split;
}

Split* Tree_splitAtPtr(Tree* tree, size_t index) {
	Split* split = malloc(sizeof(Split));
	*split = Tree_splitAt(tree, index);
	return split;
}

// }}}

// vim: set foldmethod=marker foldlevel=0 :
