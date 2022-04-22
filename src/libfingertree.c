#include "libfingertree.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifndef NDEBUG
void showInt(FILE* file, void* num) {
	fprintf(file, "%zu", (size_t)num);
}
#endif

#ifndef NDEBUG
long refCounts[3] = { 0, 0, 0 };
long refCountGet(FRefType type) { return refCounts[type]; }
#define refCountInc(type) ++refCounts[type]
#define refCountDec(type) --refCounts[type]
#else
#define refCountInc(type)
#define refCountDec(type)
#endif

// {{{ misc

FTree emptyTree = {
	.refs = 1,
	.type = FEmptyT,
	.empty = NULL
};

static int FNode_count(FNode* node) {
	assert(node->size != 1);
	if(node->items[2] == NULL)
		return 2;
	else
		return 3;
}

static FTree* FTree_fromDigit(FDigit* digit) {
	for(char i = 0; i < digit->count; ++i)
		FNode_incRef(digit->items[i]);
	switch(digit->count) {
		case 0: return FEmpty_make();
		case 1: return FSingle_make(digit->items[0]);
		case 2: return FDeep_make(digit->size, FDigit_makeNS(1, digit->items),
			FEmpty_make(), FDigit_makeNS(1, digit->items + 1));
		case 3: return FDeep_make(digit->size, FDigit_makeNS(2, digit->items),
			FEmpty_make(), FDigit_makeNS(1, digit->items + 2));
		case 4: return FDeep_make(digit->size, FDigit_makeNS(2, digit->items),
			FEmpty_make(), FDigit_makeNS(2, digit->items + 2));
		default: assert(false);
	}
}

// }}}

// {{{ incRef

FTree* FTree_incRef(FTree* tree) {
	assert(tree != NULL);
	++tree->refs;
	refCountInc(FTreeR);
	return tree;
}

FDigit* FDigit_incRef(FDigit* digit) {
	assert(digit != NULL);
	++digit->refs;
	refCountInc(FDigitR);
	return digit;
}

FNode* FNode_incRef(FNode* node) {
	if(node == NULL) return NULL;
	++node->refs;
	refCountInc(FNodeR);
	return node;
}

static FNode* FNode_incRefM(FNode* node) {
	if(node == NULL) return NULL;
	return FNode_incRef(node);
}

FIterCons* FIterCons_incRef(FIterCons* cons) {
	switch(cons->type) {
		case FNodeI:  FNode_incRef(cons->node);   break;
		case FTreeI:  FTree_incRef(cons->tree);   break;
		case FDigitI: FDigit_incRef(cons->digit); break;
		default: assert(false);
	}
	return cons;
}

// }}}

// {{{ decRef

void FNode_decRef(FNode* node) {
	assert(node != NULL);
	refCountDec(FNodeR);
	if(--node->refs == 0) {
		if(node->size > 1) {
			FNode_decRef(node->items[0]);
			FNode_decRef(node->items[1]);
			if(node->items[2] != NULL)
				FNode_decRef(node->items[2]);
		}
		free(node);
	}
}

void* FNode_decRefRet(FNode* node, void* ret) {
	FNode_decRef(node);
	return ret;
}

void FDigit_decRef(FDigit* digit) {
	assert(digit != NULL);
	refCountDec(FDigitR);
	if(--digit->refs == 0) {
		switch(digit->count) {
			case 4: FNode_decRef(digit->items[3]);
			case 3: FNode_decRef(digit->items[2]);
			case 2: FNode_decRef(digit->items[1]);
			case 1: FNode_decRef(digit->items[0]);
				break;
			default: assert(false);
		}
		free(digit);
	}
}

void* FDigit_decRefRet(FDigit* digit, void* ret) {
	FDigit_decRef(digit);
	return ret;
}

void FTree_decRef(FTree* tree) {
	assert(tree != NULL);
	refCountDec(FTreeR);
	if(--tree->refs == 0) {
		switch(tree->type) {
			case FEmptyT:
				break;
			case FSingleT:
				FNode_decRef(tree->single);
				break;
			case FDeepT:
				FDigit_decRef(tree->deep->left);
				FTree_decRef(tree->deep->middle);
				FDigit_decRef(tree->deep->right);
				free(tree->deep);
				break;
			default: assert(false);
		}
		free(tree);
	}
}

void* FTree_decRefRet(FTree* tree, void* ret) {
	FTree_decRef(tree);
	return ret;
}

void FIterCons_decRef(FIterCons* cons) {
	switch(cons->type) {
		case FNodeI: FNode_decRef(cons->node); break;
		case FTreeI: FTree_decRef(cons->tree); break;
		case FDigitI: FDigit_decRef(cons->digit); break;
		default: assert(false);
	}
}

void* FIterCons_decRefRet(FIterCons* cons, void* ret) {
	FIterCons_decRef(cons);
	return ret;
}

// }}}

// {{{ alloc

FTree* FTree_alloc() {
	FTree* tree = malloc(sizeof(FTree));
	tree->refs = 1;
	refCountInc(FTreeR);
	return tree;
}

FDeep* FDeep_alloc() {
	FDeep* deep = malloc(sizeof(FDeep));
	return deep;
}

FDigit* FDigit_alloc() {
	FDigit* digit = malloc(sizeof(FDigit));
	digit->refs = 1;
	refCountInc(FDigitR);
	return digit;
}

FNode* FNode_alloc() {
	FNode* node = malloc(sizeof(FNode));
	node->refs = 1;
	refCountInc(FNodeR);
	return node;
}

FIterCons* FIterCons_alloc() {
	return malloc(sizeof(FIterCons));
}

FIter* FIter_alloc() {
	return malloc(sizeof(FIter));
}

// }}}

// {{{ make / replace

FTree* FEmpty_make() {
	++emptyTree.refs;
	refCountInc(FTreeR);
	return &emptyTree;
}

FTree* FSingle_make(FNode* node) {
	FTree* tree = FTree_alloc();
	tree->type = FSingleT;
	tree->single = node;
	return tree;
}

FTree* FDeep_make(size_t size, FDigit* left, FTree* middle, FDigit* right) {
	FDeep* deep = FDeep_alloc();
	deep->size = size;
	deep->left = left;
	deep->middle = middle;
	deep->right = right;
	FTree* tree = FTree_alloc();
	tree->type = FDeepT;
	tree->deep = deep;
	return tree;
}

FTree* FDeep_makeS(FDigit* left, FTree* middle, FDigit* right) {
	size_t size = left->size + FTree_size(middle) + right->size;
	return FDeep_make(size, left, middle, right);
}


FDigit* FDigit_make(
	size_t size, char count,
	FNode* n0, FNode* n1, FNode* n2, FNode* n3
) {
	assert(1 <= count && count <= 4);
	FDigit* digit = FDigit_alloc();
	digit->size = size;
	digit->count = count;
	digit->items[0] = n0;
	digit->items[1] = n1; assert((count < 2) == (n1 == NULL));
	digit->items[2] = n2; assert((count < 3) == (n2 == NULL));
	digit->items[3] = n3; assert((count < 4) == (n3 == NULL));
	return digit;
}

FDigit* FDigit_makeN(size_t size, char count, FNode** nodes) {
	switch(count) {
		case 1: return FDigit_make(size,
			count, nodes[0], NULL, NULL, NULL);
		case 2: return FDigit_make(size,
			count, nodes[0], nodes[1], NULL, NULL);
		case 3: return FDigit_make(size,
			count, nodes[0], nodes[1], nodes[2], NULL);
		case 4: return FDigit_make(size,
			count, nodes[0], nodes[1], nodes[2], nodes[3]);
		default: assert(false);
	}
}

FDigit* FDigit_makeNS(char count, FNode** nodes) {
	assert(nodes[0] != NULL);
	size_t size = nodes[0]->size;
	switch(count) {
		case 4: assert(nodes[3] != NULL); size += nodes[3]->size;
		case 3: assert(nodes[2] != NULL); size += nodes[2]->size;
		case 2: assert(nodes[1] != NULL); size += nodes[1]->size;
		case 1: break;
		default: assert(false);
	}
	return FDigit_makeN(size, count, nodes);
}

FDigit* FDigit_fromNode(FNode* node) {
	return FDigit_make(node->size, FNode_count(node),
		FNode_incRef(node->items[0]),
		FNode_incRef(node->items[1]),
		FNode_incRefM(node->items[2]),
		NULL);
}

FNode* FNode_make(size_t size, FNode* n0, FNode* n1, FNode* n2) {
	FNode* node = FNode_alloc();
	node->size = size;
	node->items[0] = n0; assert(node->size == 1 || n0 != NULL);
	node->items[1] = n1; assert(n1 != NULL || size == 1);
	node->items[2] = n2;
	return node;
}

FNode* FNode_makeS(FNode* n0, FNode* n1, FNode* n2) {
	size_t size = n0->size + n1->size + (n2 == NULL ? 0 : n2->size);
	return FNode_make(size, n0, n1, n2);
}

FNode* FNode_makeNS(char count, FNode** nodes) {
	switch(count) {
		case 2: return FNode_make(
			nodes[0]->size + nodes[1]->size,
			nodes[0], nodes[1], NULL);
		case 3: return FNode_make(
			nodes[0]->size + nodes[1]->size + nodes[2]->size,
			nodes[0], nodes[1], nodes[2]);
		default: assert(false);
	}
}

FNode* FNode_make1(void* item) {
	return FNode_make(1, item, NULL, NULL);
}

FIterCons* FIterCons_make(FIterType type, void* item, FIterCons* next) {
	FIterCons* cons = FIterCons_alloc();
	cons->type = type;
	cons->index = 0;
	cons->tree = item;
	cons->next = next;
	return FIterCons_incRef(cons);
}

FIter* FIter_replace(FIter* iter, FIterType type, void* item) {
	FIterCons_decRef(iter->stack);
	iter->stack->type = type;
	iter->stack->index = 0;
	iter->stack->tree = item;
	FIterCons_incRef(iter->stack);
	return iter;
}

FIter* FIter_make(FIterCons* stack) {
	FIter* iter = FIter_alloc();
	iter->stack = stack;
	return iter;
}

// }}}

// {{{ print

#ifndef NDEBUG

static void FIndent_fprint(FILE* file, int indent) {
	// fprintf(file, "%d ", indent);
	for(int i = 0; i < indent; ++i)
		fprintf(file, "  ");
}

void FNode_fprint(
	FILE* file,
	FNode* node,
	int indent,
	void(*show)(FILE*,void*)
) {
	FIndent_fprint(file, indent);
	if(node->size == 1) {
		fprintf(file, "FElement(%zu) ", node->refs);
		show(file, node->items[0]);
		fprintf(file, "\n");
	} else {
		fprintf(file, "FNode[%zu](%zu)\n", node->size, node->refs);
		FNode_fprint(file, (FNode*)node->items[0], indent + 1, show);
		FNode_fprint(file, (FNode*)node->items[1], indent + 1, show);
		if(node->items[2] != NULL)
			FNode_fprint(file, (FNode*)node->items[2], indent + 1, show);
	}
}

void FNode_print(FNode* node) {
	FNode_fprint(stdout, node, 0, showInt);
}

void FDigit_fprint(
	FILE* file,
	FDigit* digit,
	int indent,
	void(*show)(FILE*,void*)
) {
	FIndent_fprint(file, indent);
	fprintf(file, "FDigit[%zu](%zu)\n", digit->size, digit->refs);
	for(int i = 0; i < digit->count; ++i)
		FNode_fprint(file, digit->items[i], indent + 1, show);
}

void FDigit_print(FDigit* digit) {
	FDigit_fprint(stdout, digit, 0, showInt);
}

void FTree_fprint(
	FILE* file,
	FTree* tree,
	int indent,
	void(*show)(FILE*,void*)
) {
	FIndent_fprint(file, indent);
	switch(tree->type) {
		case FEmptyT:
			fprintf(file, "FEmpty(%zu)\n", tree->refs);
			break;
		case FSingleT:
			fprintf(file, "FSingle(%zu)\n", tree->refs);
			FNode_fprint(file, tree->single, indent + 1, show);
			break;
		case FDeepT:
			fprintf(file, "FDeep[%zu](%zu)\n",
				tree->deep->size, tree->refs);
			FDigit_fprint(file, tree->deep->left, indent + 1, show);
			FTree_fprint(file, tree->deep->middle, indent + 1, show);
			FDigit_fprint(file, tree->deep->right, indent + 1, show);
			break;
		default: assert(false);
	}
}

void FTree_print(FTree* tree) {
	FTree_fprint(stdout, tree, 0, showInt);
}

static void FIterCons_fprint(
	FILE* file,
	FIterCons* cons,
	int indent,
	bool showNode,
	void(*show)(FILE*,void*)
) {
	FIndent_fprint(file, indent);
	if(cons == NULL) {
		fprintf(file, "FCons (nil)\n");
		return;
	}
	fprintf(file, "FCons %u ", cons->index);
	switch(cons->type) {
		case FTreeI:
			fprintf(file, "FTree\n");
			FIndent_fprint(file, indent + 1);
			fprintf(file, "%p\n", cons->tree);
			if(showNode) FTree_fprint(file, cons->tree, indent + 1, show);
			break;
		case FDigitI:
			fprintf(file, "FDigit\n");
			if(showNode) FDigit_fprint(file, cons->digit, indent + 1, show);
			break;
		case FNodeI:
			fprintf(file, "FNode\n");
			if(showNode) FNode_fprint(file, cons->node, indent + 1, show);
			break;
		default: assert(false);
	}
	FIterCons_fprint(file, cons->next, indent + 1, showNode, show);
}

void FIter_fprint(
	FILE* file,
	FIter* iter,
	int indent,
	bool showNode,
	void(*show)(FILE*,void*)
) {
	FIndent_fprint(file, indent);
	fprintf(file, "FIter\n");
	FIterCons_fprint(file, iter->stack, indent + 1, showNode, show);
}

void FIter_print(FIter* iter, bool showNode) {
	FIter_fprint(stdout, iter, 0, showNode, showInt);
}

#endif

// }}}

// {{{ bool, len

bool FTree_empty(FTree* tree) {
	return tree->type != FEmptyT;
}

size_t FTree_size(FTree* tree) {
	switch(tree->type) {
		case FEmptyT:  return 0;
		case FSingleT: return tree->single->size;
		case FDeepT:   return tree->deep->size;
		default:      assert(false);
	}
}

// }}}

// {{{ pull

static FView FTree_viewLeftN(FTree* tree);

FTree* FTree_pullLeft(FTree* middle, FDigit* right) {
	FView view = FTree_viewLeftN(middle);
	if(view.tree == NULL)
		return FTree_fromDigit(right);
	FTree* tail = FDeep_make(FTree_size(middle) + right->size,
		FDigit_fromNode(view.item), view.tree,
		FDigit_incRef(right));
	FNode_decRef(view.item);
	return tail;
}

static FView FTree_viewRightN(FTree* tree);

FTree* FTree_pullRight(FTree* middle, FDigit* left) {
	FView view = FTree_viewRightN(middle);
	if(view.tree == NULL)
		return FTree_fromDigit(left);
	FTree* init = FDeep_make(FTree_size(middle) + left->size,
		FDigit_incRef(left), view.tree,
		FDigit_fromNode(view.item));
	FNode_decRef(view.item);
	return init;
}

// }}}

// {{{ appendLeft

static FDigit* FDigit_appendLeftN(FDigit* digit, FNode* node) {
	assert(digit->count < 4);
	switch(digit->count) {
		case 3: return FDigit_make(digit->size + node->size, 4, node,
			FNode_incRef(digit->items[0]),
			FNode_incRef(digit->items[1]),
			FNode_incRef(digit->items[2]));
		case 2: return FDigit_make(digit->size + node->size, 3, node,
			FNode_incRef(digit->items[0]),
			FNode_incRef(digit->items[1]), NULL);
		case 1: return FDigit_make(digit->size + node->size, 2, node,
			FNode_incRef(digit->items[0]), NULL, NULL);
		default: assert(false);
	}
}

FTree* FTree_appendLeftN(FTree* tree, FNode* node) {
	switch(tree->type) {
		case FEmptyT:
			return FSingle_make(node);
		case FSingleT:
			return FDeep_make(tree->single->size + node->size,
				FDigit_make(node->size, 1, node, NULL, NULL, NULL),
				FEmpty_make(),
				FDigit_make(tree->single->size, 1,
					FNode_incRef(tree->single), NULL, NULL, NULL));
		case FDeepT:
			if(tree->deep->left->count < 4)
				return FDeep_make(tree->deep->size + node->size,
					FDigit_appendLeftN(tree->deep->left, node),
					FTree_incRef(tree->deep->middle),
					FDigit_incRef(tree->deep->right));
			return FDeep_make(tree->deep->size + node->size,
				FDigit_make(tree->deep->left->items[0]->size + node->size, 2,
					node, FNode_incRef(tree->deep->left->items[0]), NULL, NULL),
				FTree_appendLeftN(tree->deep->middle, FNode_make(
					tree->deep->left->size - tree->deep->left->items[0]->size,
					FNode_incRef(tree->deep->left->items[1]),
					FNode_incRef(tree->deep->left->items[2]),
					FNode_incRef(tree->deep->left->items[3]))),
				FDigit_incRef(tree->deep->right));
		default: assert(false);
	}
}

FTree* FTree_appendLeft(FTree* tree, void* item) {
	return FTree_appendLeftN(tree, FNode_make1(item));
}

// }}}

// {{{ appendRight

static FDigit* FDigit_appendRightN(FDigit* digit, FNode* node) {
	assert(digit->count < 4);
	switch(digit->count) {
		case 3: return FDigit_make(digit->size + node->size, 4,
			FNode_incRef(digit->items[0]),
			FNode_incRef(digit->items[1]),
			FNode_incRef(digit->items[2]),
			node);
		case 2: return FDigit_make(digit->size + node->size, 3,
			FNode_incRef(digit->items[0]),
			FNode_incRef(digit->items[1]),
			node, NULL);
		case 1: return FDigit_make(digit->size + node->size, 2,
			FNode_incRef(digit->items[0]),
			node, NULL, NULL);
		default: assert(false);
	}
}

static FTree* FTree_appendRightN(FTree* tree, FNode* node) {
	switch(tree->type) {
		case FEmptyT:
			return FSingle_make(node);
		case FSingleT:
			return FDeep_make(tree->single->size + node->size,
				FDigit_make(tree->single->size, 1,
					FNode_incRef(tree->single), NULL, NULL, NULL),
				FEmpty_make(),
				FDigit_make(node->size, 1, node, NULL, NULL, NULL));
		case FDeepT:
			if(tree->deep->right->count < 4)
				return FDeep_make(tree->deep->size + node->size,
					FDigit_incRef(tree->deep->left),
					FTree_incRef(tree->deep->middle),
					FDigit_appendRightN(tree->deep->right, node));
			return FDeep_make(tree->deep->size + node->size,
				FDigit_incRef(tree->deep->left),
				FTree_appendRightN(tree->deep->middle, FNode_make(
					tree->deep->right->size - tree->deep->right->items[3]->size ,
					FNode_incRef(tree->deep->right->items[0]),
					FNode_incRef(tree->deep->right->items[1]),
					FNode_incRef(tree->deep->right->items[2]))),
				FDigit_make(tree->deep->right->items[3]->size + node->size,
					2, FNode_incRef(tree->deep->right->items[3]),
					node, NULL, NULL));
		default: assert(false);
	}
}

FTree* FTree_appendRight(FTree* tree, void* item) {
	return FTree_appendRightN(tree, FNode_make1(item));
}

// }}}

// {{{ viewLeft

static FView FTree_viewLeftN(FTree* tree) {
	assert(tree != NULL);
	switch(tree->type) {
		case FEmptyT: return (FView){NULL, NULL};
		case FSingleT: return (FView){FNode_incRef(tree->single), FEmpty_make()};
		case FDeepT: {
			FDigit* left = tree->deep->left;
			FNode* head = FNode_incRef(left->items[0]);
			if(left->count == 1) return (FView){ head,
				FTree_pullLeft(tree->deep->middle, tree->deep->right) };
			for(char i = 1; i < left->count; ++i)
				FNode_incRef(left->items[i]);
			FTree* tail = FDeep_make(tree->deep->size - head->size,
				FDigit_make(left->size - head->size, left->count - 1,
					left->items[1], left->items[2], left->items[3], NULL),
				FTree_incRef(tree->deep->middle),
				FDigit_incRef(tree->deep->right));
			return (FView){head, tail};
		}
		default: assert(false);
	}
}

FView FTree_viewLeft(FTree* tree) {
	FView view = FTree_viewLeftN(tree);
	if(view.tree != NULL) {
		FNode* node = view.item;
		assert(node->size == 1);
		view.item = node->value;
		FNode_decRef(node);
	}
	return view;
}

FView* FTree_viewLeftPtr(FTree* tree) {
	FView* view = malloc(sizeof(FView));
	*view = FTree_viewLeft(tree);
	return view;
}

// }}}

// {{{ viewRight

static FView FTree_viewRightN(FTree* tree) {
	assert(tree != NULL);
	switch(tree->type) {
		case FEmptyT: return (FView){NULL, NULL};
		case FSingleT: return (FView){FNode_incRef(tree->single), FEmpty_make()};
		case FDeepT: {
			FDigit* right = tree->deep->right;
			FNode* last = FNode_incRef(right->items[right->count-1]);
			if(right->count == 1) return (FView){ last,
				FTree_pullRight(tree->deep->middle, tree->deep->left) };
			for(char i = 0; i < right->count - 1; ++i)
				FNode_incRef(right->items[i]);
			FTree* init = FDeep_make(tree->deep->size - last->size,
				FDigit_incRef(tree->deep->left),
				FTree_incRef(tree->deep->middle),
				FDigit_makeN(right->size - last->size,
					right->count - 1, right->items));
			return (FView){last, init};
		}
		default: assert(false);
	}
}

FView FTree_viewRight(FTree* tree) {
	FView view = FTree_viewRightN(tree);
	if(view.tree != NULL) {
		FNode* node = view.item;
		assert(node->size == 1);
		view.item = node->value;
		FNode_decRef(node);
	}
	return view;
}

FView* FTree_viewRightPtr(FTree* tree) {
	FView* view = malloc(sizeof(FView));
	*view = FTree_viewRight(tree);
	return view;
}

// }}}

// {{{ fromArray

static FTree* FTree_fromNodes(size_t size, size_t count, FNode** nodes) {
	if(count == 0) return FEmpty_make();
	if(count == 1) return FSingle_make(nodes[0]);
	if(count <= 8) return FDeep_make(size,
		FDigit_makeNS(count >> 1, nodes), FEmpty_make(),
		FDigit_makeNS(count - (count >> 1), nodes + (count >> 1)));
	size_t countN = (count + 2) / 3 - 2;
	FNode** nodesN = malloc(countN * sizeof(FNode*));
	for(size_t i = 2, j = 3; i < countN; ++i, j += 3)
		nodesN[i-2] = FNode_makeS(nodes[j], nodes[j+1], nodes[j+2]);
	switch(count % 3) {
		case 0:
			assert(countN >= 1);
			if(countN >= 2)
				nodesN[countN-2] = FNode_makeS(
					nodes[count-9], nodes[count-8], nodes[count-7]);
			nodesN[countN-1] = FNode_makeS(
				nodes[count-6], nodes[count-5], nodes[count-4]);
			break;
		case 1:
			assert(countN >= 2);
			nodesN[countN-2] = FNode_makeS(
				nodes[count-7], nodes[count-6], NULL);
			nodesN[countN-1] = FNode_makeS(
				nodes[count-5], nodes[count-4], NULL);
			break;
		case 2:
			assert(countN >= 2);
			nodesN[countN-2] = FNode_makeS(
				nodes[count-8], nodes[count-7], nodes[count-6]);
			nodesN[countN-1] = FNode_makeS(
				nodes[count-5], nodes[count-4], NULL);
			break;
		default: assert(false);
	}
	FDigit* left = FDigit_make(
		nodes[0]->size + nodes[1]->size + nodes[2]->size,
		3, nodes[0], nodes[1], nodes[2], NULL);
	FDigit* right = FDigit_make(
		nodes[count-3]->size + nodes[count-2]->size + nodes[count-3]->size,
		3, nodes[count-3], nodes[count-2], nodes[count-1], NULL);
	FTree* tree = FDeep_make(size, left,
		FTree_fromNodes(count - 6, countN, nodesN), right);
	free(nodesN);
	return tree;
}

FTree* FTree_fromArray(size_t size, void** items) {
	FNode** nodes = malloc(size * sizeof(FNode*));
	for(size_t i = 0; i < size; ++i)
		nodes[i] = FNode_make1(items[i]);
	FTree* tree = FTree_fromNodes(size, size, nodes);
	free(nodes);
	return tree;
}

// }}}

// {{{ iteration

static FIter* FIter_pushStack(FIter* iter, FIterType type, void* item) {
	iter->stack = FIterCons_make(type, item, iter->stack);
	return iter;
}

static FIter* FIter_popStack(FIter* iter) {
	assert(iter->stack != NULL);
	FIterCons* cons = iter->stack;
	FIterCons_decRef(cons);
	iter->stack = cons->next;
	free(cons);
	return iter;
}

bool FIter_empty(FIter* iter) {
	return iter->stack == NULL;
}

static FIter* FIter_advance(FIter* iter) {
	if(iter->stack == NULL) return NULL;
	switch(iter->stack->type) {
		case FTreeI:
			switch(iter->stack->tree->type) {
				case FEmptyT:
					assert(iter->stack->index == 0);
					return FIter_advance(FIter_popStack(iter));
				case FSingleT:
					assert(iter->stack->index == 0);
					return FIter_advance(FIter_replace(iter, FNodeI,
						iter->stack->tree->single));
				case FDeepT:
					switch(iter->stack->index++) {
						case 0:
							return FIter_advance(FIter_pushStack(iter, FDigitI,
								iter->stack->tree->deep->left));
						case 1:
							return FIter_advance(FIter_pushStack(iter, FTreeI,
								iter->stack->tree->deep->middle));
						case 2:
							return FIter_advance(FIter_replace(iter, FDigitI,
								iter->stack->tree->deep->right));
						default: assert(false);
					}
				default: assert(false);
			};
		case FDigitI:
			assert(iter->stack->index <= 4);
			if(iter->stack->index == iter->stack->digit->count)
				return FIter_advance(FIter_popStack(iter));
			return FIter_advance(FIter_pushStack(iter, FNodeI,
				iter->stack->digit->items[iter->stack->index++]));
		case FNodeI:
			if(iter->stack->node->size == 1) {
				assert(iter->stack->index == 0);
				return iter;
			}
			assert(iter->stack->index <= 3);
			if(iter->stack->index == FNode_count(iter->stack->node))
				return FIter_advance(FIter_popStack(iter));
			return FIter_advance(FIter_pushStack(iter, FNodeI,
				iter->stack->node->items[iter->stack->index++]));
		default: assert(false);
	}
}

void* FIter_next(FIter* iter) {
	assert(!FIter_empty(iter));
	FNode* node = iter->stack->node;
	assert(node->size == 1);
	FIter_popStack(iter);
	FIter_advance(iter);
	return node->value;
}

FIter* FIter_fromTree(FTree* tree) {
	switch(tree->type) {
		case FEmptyT: return FIter_make(NULL);
		default: {
			FIter* iter = FIter_make(FIterCons_make(FTreeI, tree, NULL));
			FIter_advance(iter);
			return iter;
		}
	}
}

// }}}

// {{{ toArray

static void** FNode_toArrayItems(FNode* node, void** items) {
	assert(node != NULL);
	if(node->size == 1) {
		*items = node->value;
		return ++items;
	}
	items = FNode_toArrayItems(node->items[0], items);
	items = FNode_toArrayItems(node->items[1], items);
	if(node->items[2] != NULL)
		items = FNode_toArrayItems(node->items[2], items);
	return items;
}

static void** FDigit_toArrayItems(FDigit* digit, void** items) {
	assert(digit != NULL);
	for(char i = 0; i < digit->count; ++i)
		items = FNode_toArrayItems(digit->items[i], items);
	return items;
}

static void** FTree_toArrayItems(FTree* tree, void** items) {
	assert(tree != NULL);
	switch(tree->type) {
		case FEmptyT: return items;
		case FSingleT: return FNode_toArrayItems(tree->single, items);
		case FDeepT:
			items = FDigit_toArrayItems(tree->deep->left, items);
			items = FTree_toArrayItems(tree->deep->middle, items);
			return FDigit_toArrayItems(tree->deep->right, items);
		default: assert(false);
	}
}

void** FTree_toArray(FTree* tree) {
	size_t size = FTree_size(tree);
	void** items = malloc(size * sizeof(void*));
	void** end = FTree_toArrayItems(tree, items);
	assert(items + size == end);
	return items;
}

// }}}

// {{{ extend

FTree* FTree_extend(FTree* xs, FTree* ys) {
	switch(xs->type) {
		case FEmptyT: return FTree_incRef(ys);
		case FSingleT: return FTree_appendLeftN(ys, FNode_incRef(xs->single));
		case FDeepT: switch(ys->type) {
			case FEmptyT: return FTree_incRef(xs);
			case FSingleT: return FTree_appendRightN(xs, FNode_incRef(ys->single));
			case FDeepT: {
				size_t size = xs->deep->size + ys->deep->size;
				FNode* mid[8]; char count = 0;
				for(; count < xs->deep->right->count; ++count)
					mid[count] = FNode_incRef(xs->deep->right->items[count]);
				for(char i = 0; i < ys->deep->left->count; ++i, ++count)
					mid[count] = FNode_incRef(ys->deep->left->items[i]);
				FTree* right = FTree_incRef(ys->deep->middle);
				switch(count) {
					case 8: right = FTree_decRefRet(right, FTree_appendLeftN(right,
						FNode_makeS(mid[5], mid[6], mid[7])));
					case 5: right = FTree_decRefRet(right, FTree_appendLeftN(right,
						FNode_makeS(mid[2], mid[3], mid[4])));
					case 2: right = FTree_decRefRet(right, FTree_appendLeftN(right,
						FNode_makeS(mid[0], mid[1], NULL)));
					break;
					case 6: right = FTree_decRefRet(right, FTree_appendLeftN(right,
						FNode_makeS(mid[3], mid[4], mid[5])));
					case 3: right = FTree_decRefRet(right, FTree_appendLeftN(right,
						FNode_makeS(mid[0], mid[1], mid[2])));
					break;
					case 7: right = FTree_decRefRet(right, FTree_appendLeftN(right,
						FNode_makeS(mid[4], mid[5], mid[6])));
					case 4:
						right = FTree_decRefRet(right, FTree_appendLeftN(right,
							FNode_makeS(mid[2], mid[3], NULL)));
						right = FTree_decRefRet(right, FTree_appendLeftN(right,
							FNode_makeS(mid[0], mid[1], NULL)));
					break;
					default: assert(false);
				}
				return FTree_decRefRet(right, FDeep_make(size,
					FDigit_incRef(xs->deep->left),
					FTree_extend(xs->deep->middle, right),
					FDigit_incRef(ys->deep->right)));
			}
			default: assert(false);
		}
		default: assert(false);
	}
}

// }}}

// {{{ index

static void* FNode_index(FNode* node, size_t index) {
	assert(node != NULL);
	assert(index < node->size);
	if(node->size == 1)
		return node->value;
	if(index < node->items[0]->size)
		return FNode_index(node->items[0], index);
	index -= node->items[0]->size;
	if(index < node->items[1]->size)
		return FNode_index(node->items[1], index);
	index -= node->items[1]->size;
	return FNode_index(node->items[2], index);
}

static void* FDigit_index(FDigit* digit, size_t index) {
	assert(index < digit->size);
	for(char i = 0; i < digit->count; ++i)
		if(index < digit->items[i]->size)
			return FNode_index(digit->items[i], index);
		else
			index -= digit->items[i]->size;
	assert(false);
}

void* FTree_index(FTree* tree, size_t index) {
	assert(index < FTree_size(tree));
	switch(tree->type) {
		case FSingleT: return FNode_index(tree->single, index);
		case FDeepT:
			if(index < tree->deep->left->size)
				return FDigit_index(tree->deep->left, index);
			index -= tree->deep->left->size;
			if(index < FTree_size(tree->deep->middle))
				return FTree_index(tree->deep->middle, index);
			index -= FTree_size(tree->deep->middle);
			return FDigit_index(tree->deep->right, index);
		default: assert(false);
	}
}

// }}}

// {{{ update

static FNode* FNode_update(FNode* node, size_t index, void* value) {
	assert(index < node->size);
	if(node->size == 1)
		return FNode_make1(value);
	if(index < node->items[0]->size)
		return FNode_make(node->size,
			FNode_update(node->items[0], index, value),
			FNode_incRef(node->items[1]),
			FNode_incRefM(node->items[2]));
	index -= node->items[0]->size;
	if(index < node->items[1]->size)
		return FNode_make(node->size,
			FNode_incRef(node->items[0]),
			FNode_update(node->items[1], index, value),
			FNode_incRefM(node->items[2]));
	index -= node->items[1]->size;
	return FNode_make(node->size,
		FNode_incRef(node->items[0]),
		FNode_incRefM(node->items[1]),
		FNode_update(node->items[2], index, value));
}

static FDigit* FDigit_update(FDigit* digit, size_t index, void* value) {
	assert(index < digit->size);
	FNode* items[4] = {NULL, NULL, NULL, NULL};
	for(char i = 0; i < digit->count; ++i)
		if(index < digit->items[i]->size) {
			items[i] = FNode_update(digit->items[i], index, value);
			for(char j = i + 1; j < digit->count; ++j)
				items[j] = FNode_incRef(digit->items[j]);
			return FDigit_make(digit->size, digit->count,
				items[0], items[1], items[2], items[3]);
		} else {
			items[i] = FNode_incRef(digit->items[i]);
			index -= digit->items[i]->size;
		}
	assert(false);
}

FTree* FTree_update(FTree* tree, size_t index, void* value) {
	assert(index < FTree_size(tree));
	switch(tree->type) {
		case FEmptyT: return FEmpty_make();
		case FSingleT: return FSingle_make(FNode_update(tree->single, index, value));
		case FDeepT:
			if(index < tree->deep->left->size)
				return FDeep_make(tree->deep->size,
					FDigit_update(tree->deep->left, index, value),
					FTree_incRef(tree->deep->middle),
					FDigit_incRef(tree->deep->right));
			index -= tree->deep->left->size;
			if(index < FTree_size(tree->deep->middle))
				return FDeep_make(tree->deep->size,
					FDigit_incRef(tree->deep->left),
					FTree_update(tree->deep->middle, index, value),
					FDigit_incRef(tree->deep->right));
			index -= FTree_size(tree->deep->middle);
			return FDeep_make(tree->deep->size,
				FDigit_incRef(tree->deep->left),
				FTree_incRef(tree->deep->middle),
				FDigit_update(tree->deep->right, index, value));
		default: assert(false);
	}
}

// }}}

// {{{ splitAt

static FSplit FTree_splitAtN(FTree* tree, size_t index);

static FSplit FDeep_splitLeftN(FDeep* deep, size_t index) {
	size_t size, dsize = 0;
	FNode* prefix[4] = { NULL, NULL, NULL, NULL };
	for(char i = 0; i < deep->left->count; ++i)
		if(index >= (size = deep->left->items[i]->size)) {
			prefix[i] = FNode_incRef(deep->left->items[i]);
			index -= size; dsize += size;
		} else if(i + 1 == deep->left->count) {
			return (FSplit){
				FTree_fromNodes(dsize, i, prefix),
				FNode_incRef(deep->left->items[i]),
				FTree_pullLeft(deep->middle, deep->right) };
		} else {
			FNode* suffix[4] = { NULL, NULL, NULL, NULL };
			for(char j = i + 1, k = 0; j < deep->left->count; ++j, ++k)
				suffix[k] = FNode_incRef(deep->left->items[j]);
			return (FSplit){
				FTree_fromNodes(dsize, i, prefix),
				FNode_incRef(deep->left->items[i]),
				FDeep_make(deep->size - dsize - size,
					FDigit_makeN(deep->left->size - dsize - size,
						deep->left->count - i - 1, suffix),
					FTree_incRef(deep->middle),
					FDigit_incRef(deep->right)) };
		}
}

static FSplit FDeep_splitRightN(FDeep* deep, size_t index) {
	size_t size, dsize = 0;
	FNode* prefix[4] = { NULL, NULL, NULL, NULL };
	for(char i = 0; i < deep->right->count; ++i)
		if(index >= (size = deep->right->items[i]->size)) {
			prefix[i] = FNode_incRef(deep->right->items[i]);
			index -= size; dsize += size;
		} else if(i == 0) {
			for(char j = 1; j < deep->right->count; ++j)
				prefix[j-1] = FNode_incRef(deep->right->items[j]);
			return (FSplit){
				FTree_pullRight(deep->middle, deep->left),
				FNode_incRef(deep->right->items[0]),
				FTree_fromNodes(deep->right->size - size,
					deep->right->count - 1, prefix) };
		} else {
			FNode* suffix[4] = { NULL, NULL, NULL, NULL };
			for(char j = i + 1, k = 0; j < deep->right->count; ++j, ++k)
				suffix[k] = FNode_incRef(deep->right->items[j]);
			return (FSplit){
				FDeep_make(deep->size - deep->right->size + dsize,
					FDigit_incRef(deep->left),
					FTree_incRef(deep->middle),
					FDigit_makeN(dsize, i, prefix)),
				FNode_incRef(deep->right->items[i]),
				FTree_fromNodes(deep->size - dsize - size,
					deep->right->count - i - 1, suffix) };
		}
}

static FSplit FDeep_splitMiddleN(FDeep* deep, size_t index) {
	FSplit split = FTree_splitAtN(deep->middle, index);
	if(split.node->size == 1) {
		FTree* left = FTree_decRefRet(split.left,
			FTree_pullRight(split.left, deep->left));
		FTree* right = FTree_decRefRet(split.right,
			FTree_pullRight(split.right, deep->right));
		return (FSplit){ left, split.node, right };
	}
	index -= FTree_size(split.left);
	size_t size, presize = 0;
	if(index < (size = split.node->items[0]->size)) {
		FTree* left = FTree_decRefRet(split.left,
			FTree_pullRight(split.left, deep->left));
		FNode* middle = FNode_incRef(split.node->items[0]);
		FTree* right = FDeep_makeS(
			FDigit_make(split.node->size - size,
				FNode_count(split.node) - 1,
				FNode_incRef(split.node->items[1]),
				FNode_incRefM(split.node->items[2]), NULL, NULL),
			split.right, FDigit_incRef(deep->right));
		FNode_decRef(split.node);
		return (FSplit){ left, middle, right };
	}
	index -= size; presize += size;
	if(index < (size = split.node->items[1]->size)) {
		FTree* left = FDeep_makeS(
			FDigit_incRef(deep->left), split.left,
			FDigit_make(split.node->items[0]->size, 1,
				FNode_incRef(split.node->items[0]), NULL, NULL, NULL));
		FNode* middle = FNode_incRef(split.node->items[1]);
		FTree* right = split.node->items[2] == NULL
			? FTree_decRefRet(split.right,
				FTree_pullLeft(split.right, deep->right))
			: FDeep_makeS(FDigit_make(split.node->items[2]->size, 1,
					FNode_incRefM(split.node->items[2]), NULL, NULL, NULL),
				split.right, FDigit_incRef(deep->right));
		FNode_decRef(split.node);
		return (FSplit){ left, middle, right };
	}
	index -= size; presize += size;
	assert(split.node->items[2] != NULL); {
		size = split.node->items[2]->size;
		FTree* left = FDeep_makeS(
			FDigit_incRef(deep->left), split.left,
			FDigit_make(split.node->size - size, 2,
				FNode_incRef(split.node->items[0]),
				FNode_incRef(split.node->items[1]), NULL, NULL));
		FNode* middle = FNode_incRef(split.node->items[2]);
		FTree* right = FTree_decRefRet(split.right,
			FTree_pullLeft(split.right, deep->right));
		FNode_decRef(split.node);
		return (FSplit){ left, middle, right };
	}
}

static FSplit FTree_splitAtN(FTree* tree, size_t index) {
	assert(index < FTree_size(tree));
	switch(tree->type) {
		case FSingleT: return (FSplit){ FEmpty_make(),
			FNode_incRef(tree->single), FEmpty_make() };
		case FDeepT:
			if(index < tree->deep->left->size)
				return FDeep_splitLeftN(tree->deep, index);
			index -= tree->deep->left->size;
			if(index < FTree_size(tree->deep->middle))
				return FDeep_splitMiddleN(tree->deep, index);
			index -= FTree_size(tree->deep->middle);
			return FDeep_splitRightN(tree->deep, index);
		default: assert(false);
	}
}

FSplit FTree_splitAt(FTree* tree, size_t index) {
	FSplit split = FTree_splitAtN(tree, index);
	assert(split.node->size == 1);
	split.item = FNode_decRefRet(split.node, split.node->items[0]);
	return split;
}

FSplit* FTree_splitAtPtr(FTree* tree, size_t index) {
	FSplit* split = malloc(sizeof(FSplit));
	*split = FTree_splitAt(tree, index);
	return split;
}

// }}}

// {{{ replicate

static FDigit* FDigit_replicateN(size_t count, FNode* node) {
	switch(count) {
		case 1: return FDigit_make(node->size, 1,
			FNode_incRef(node), NULL, NULL, NULL);
		case 2: return FDigit_make(2 * node->size, 2,
			FNode_incRef(node), FNode_incRef(node), NULL, NULL);
		case 3: return FDigit_make(3 * node->size, 3,
			FNode_incRef(node), FNode_incRef(node), FNode_incRef(node), NULL);
		case 4: return FDigit_make(4 * node->size, 4,
			FNode_incRef(node), FNode_incRef(node),
			FNode_incRef(node), FNode_incRef(node));
		default: assert(false);
	}
}

static FTree* FTree_replicateN(size_t count, FNode* node) {
	if(count == 0) return FEmpty_make();
	if(count == 1) return FSingle_make(FNode_incRef(node));
	if(count <= 8) return FDeep_make(count * node->size,
		FDigit_replicateN(count >> 1, node), FEmpty_make(),
		FDigit_replicateN(count - (count >> 1), node));
	FDigit *left, *right;
	size_t countN = count / 3 - 1;
	switch(count % 3) {
		case 0:
			--countN;
			left = right = FDigit_incRef(FDigit_replicateN(3, node));
			break;
		case 1:
			left = right = FDigit_incRef(FDigit_replicateN(2, node));
			break;
		case 2:
			left = FDigit_replicateN(3, node);
			right = FDigit_replicateN(2, node);
			break;
		default: assert(false);
	}
	FNode* nodeN = FNode_make(3 * node->size,
		FNode_incRef(node), FNode_incRef(node), FNode_incRef(node));
	return FNode_decRefRet(nodeN, FDeep_make(count * node->size,
		left, FTree_replicateN(countN, nodeN), right));
}

FTree* FTree_replicate(size_t count, void* item) {
	FNode* node = FNode_make1(item);
	return FNode_decRefRet(node, FTree_replicateN(count, node));
}

// }}}

// vim: set foldmethod=marker foldlevel=0 :
