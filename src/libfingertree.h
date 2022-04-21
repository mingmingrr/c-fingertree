#pragma once

#include <stdio.h>
#include <stdbool.h>

typedef enum RefType {
	TreeR = 0,
	NodeR = 1,
	DigitR = 2
} RefType;

#ifndef NDEBUG

extern long refCounts[3];

long refCountGet(RefType type);

void showInt(FILE* file, void* num);

#endif

typedef struct Node {
	size_t refs;
	size_t size;
	union {
		void* value;
		struct Node* items[3];
	};
} Node;

typedef struct Digit {
	size_t   refs;
	size_t   size;
	char     count;
	Node*    items[4];
} Digit;

typedef struct Tree Tree;

typedef struct Deep {
	size_t size;
	Digit* left;
	Tree*  middle;
	Digit* right;
} Deep;

typedef enum TreeType {
	EmptyT  = 0,
	SingleT = 1,
	DeepT   = 2
} TreeType;

struct Tree {
	size_t refs;
	TreeType type;
	union {
		void* empty;
		Node* single;
		Deep* deep;
	};
};

typedef enum IterType {
	TreeI  = 0,
	DigitI = 1,
	NodeI  = 2
} IterType;

typedef struct IterCons {
	IterType type;
	unsigned index;
	union {
		Tree* tree;
		Node* node;
		Digit* digit;
	};
	struct IterCons* next;
} IterCons;

typedef struct Iter {
	IterCons* stack;
} Iter;

typedef struct View {
	void* item;
	Tree* tree;
} View;

typedef struct Split {
	Tree* left;
	union {
		void* item;
		Node* node;
	};
	Tree* right;
} Split;

Tree* Tree_incRef(Tree* tree);

Digit* Digit_incRef(Digit* digit);

Node* Node_incRef(Node* node);

IterCons* IterCons_incRef(IterCons* cons);

void Node_decRef(Node* node);

void* Node_decRefRet(Node* node, void* ret);

void Digit_decRef(Digit* digit);

void* Digit_decRefRet(Digit* digit, void* ret);

void Tree_decRef(Tree* tree);

void* Tree_decRefRet(Tree* tree, void* ret);

void IterCons_decRef(IterCons* cons);

void* IterCons_decRefRet(IterCons* cons, void* ret);

Tree* Tree_alloc();

Deep* Deep_alloc();

Digit* Digit_alloc();

Node* Node_alloc();

IterCons* IterCons_alloc();

Iter* Iter_alloc();

Tree* Empty_make();

Tree* Single_make(Node* node);

Tree* Deep_make(size_t size, Digit* left, Tree* middle, Digit* right);

Tree* Deep_makeS(Digit* left, Tree* middle, Digit* right);

Digit* Digit_make(
	size_t size, char count,
	Node* n0, Node* n1, Node* n2, Node* n3
);

Digit* Digit_makeN(size_t size, char count, Node** nodes);

Digit* Digit_makeNS(char count, Node** nodes);

Digit* Digit_fromNode(Node* node);

Node* Node_make(size_t size, Node* n0, Node* n1, Node* n2);

Node* Node_makeS(Node* n0, Node* n1, Node* n2);

Node* Node_makeNS(char count, Node** nodes);

Node* Node_make1(void* item);

IterCons* IterCons_make(IterType type, void* item, IterCons* next);

Iter* Iter_replace(Iter* iter, IterType type, void* item);

Iter* Iter_make(IterCons* stack);

#ifndef NDEBUG

void Digit_fprint(FILE*, Digit*, int indent, void(*show)(FILE*,void*));

void Digit_print(Digit*);

void Node_fprint(FILE*, Node*, int indent, void(*show)(FILE*,void*));

void Node_print(Node*);

void Tree_fprint(FILE*, Tree*, int indent, void(*show)(FILE*,void*));

void Tree_print(Tree*);

void Iter_fprint(FILE*, Iter*, int indent, bool showNode, void(*show)(FILE*,void*));

void Iter_print(Iter*, bool showNode);

#endif

bool Tree_empty(Tree* tree);

size_t Tree_size(Tree* tree);

Tree* Tree_appendLeft(Tree* tree, void* item);

Tree* Tree_appendRight(Tree* tree, void* item);

View Tree_viewLeft(Tree* tree);

View* Tree_viewLeftPtr(Tree* tree);

View Tree_viewRight(Tree* tree);

View* Tree_viewRightPtr(Tree* tree);

Tree* Tree_fromArray(size_t size, void** items);

bool Iter_empty(Iter* iter);

void* Iter_next(Iter* iter);

Iter* Iter_fromTree(Tree* tree);

void** Tree_toArray(Tree* tree);

void* Tree_index(Tree* tree, size_t index);

Tree* Tree_update(Tree* tree, size_t index, void* value);

Split Tree_splitAt(Tree* tree, size_t index);

Split* Tree_splitAtPtr(Tree* tree, size_t index);

Tree* Tree_replicate(size_t count, void* item);

// vim: set foldmethod=marker foldlevel=0 :
