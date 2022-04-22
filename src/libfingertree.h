#pragma once

#include <stdio.h>
#include <stdbool.h>

typedef enum FRefType {
	FTreeR = 0,
	FNodeR = 1,
	FDigitR = 2
} FRefType;

#ifndef NDEBUG

extern long refCounts[3];

long refCountGet(FRefType type);

void showInt(FILE* file, void* num);

#endif

typedef struct FNode {
	size_t refs;
	size_t size;
	union {
		void* value;
		struct FNode* items[3];
	};
} FNode;

typedef struct FDigit {
	size_t refs;
	size_t size;
	char   count;
	FNode* items[4];
} FDigit;

typedef struct FTree FTree;

typedef struct FDeep {
	size_t  size;
	FDigit* left;
	FTree*  middle;
	FDigit* right;
} FDeep;

typedef enum FTreeType {
	FEmptyT  = 0,
	FSingleT = 1,
	FDeepT   = 2
} FTreeType;

struct FTree {
	size_t    refs;
	FTreeType type;
	union {
		void* empty;
		FNode* single;
		FDeep* deep;
	};
};

typedef enum FIterType {
	FTreeI  = 0,
	FDigitI = 1,
	FNodeI  = 2
} FIterType;

typedef struct FIterCons {
	FIterType type;
	unsigned  index;
	union {
		FTree*  tree;
		FNode*  node;
		FDigit* digit;
	};
	struct FIterCons* next;
} FIterCons;

typedef struct FIter {
	FIterCons* stack;
} FIter;

typedef struct FView {
	void*  item;
	FTree* tree;
} FView;

typedef struct FSplit {
	FTree* left;
	union {
		void* item;
		FNode* node;
	};
	FTree* right;
} FSplit;

// ref counting

FTree* FTree_incRef(FTree* tree);

FDigit* FDigit_incRef(FDigit* digit);

FNode* FNode_incRef(FNode* node);

FIterCons* FIterCons_incRef(FIterCons* cons);

void FNode_decRef(FNode* node);

void* FNode_decRefRet(FNode* node, void* ret);

void FDigit_decRef(FDigit* digit);

void* FDigit_decRefRet(FDigit* digit, void* ret);

void FTree_decRef(FTree* tree);

void* FTree_decRefRet(FTree* tree, void* ret);

void FIterCons_decRef(FIterCons* cons);

void* FIterCons_decRefRet(FIterCons* cons, void* ret);

// allocate and fill default fields

FTree* FTree_alloc();

FDeep* FDeep_alloc();

FDigit* FDigit_alloc();

FNode* FNode_alloc();

FIterCons* FIterCons_alloc();

FIter* FIter_alloc();

// smart constructors

FTree* FEmpty_make();

FTree* FSingle_make(FNode* node);

FTree* FDeep_make(size_t size, FDigit* left, FTree* middle, FDigit* right);

FTree* FDeep_makeS(FDigit* left, FTree* middle, FDigit* right);

FDigit* FDigit_make(
	size_t size, char count,
	FNode* n0, FNode* n1, FNode* n2, FNode* n3
);

FDigit* FDigit_makeN(size_t size, char count, FNode** nodes);

FDigit* FDigit_makeNS(char count, FNode** nodes);

FDigit* FDigit_fromNode(FNode* node);

FNode* FNode_make(size_t size, FNode* n0, FNode* n1, FNode* n2);

FNode* FNode_makeS(FNode* n0, FNode* n1, FNode* n2);

FNode* FNode_makeNS(char count, FNode** nodes);

FNode* FNode_make1(void* item);

FIterCons* FIterCons_make(FIterType type, void* item, FIterCons* next);

FIter* FIter_replace(FIter* iter, FIterType type, void* item);

FIter* FIter_make(FIterCons* stack);

// print structures

#ifndef NDEBUG

void FDigit_fprint(FILE*, FDigit*, int indent, void(*show)(FILE*,void*));

void FDigit_print(FDigit*);

void FNode_fprint(FILE*, FNode*, int indent, void(*show)(FILE*,void*));

void FNode_print(FNode*);

void FTree_fprint(FILE*, FTree*, int indent, void(*show)(FILE*,void*));

void FTree_print(FTree*);

void FIter_fprint(FILE*, FIter*, int indent, bool showN, void(*show)(FILE*,void*));

void FIter_print(FIter*, bool showNode);

#endif

// check if a tree does not contain any items
// O(1)
bool FTree_empty(FTree* tree);

// get number of items in a tree
// O(1)
size_t FTree_size(FTree* tree);

// append an element on the left side
// amortized O(1)
// creates a new FTree
FTree* FTree_appendLeft(FTree* tree, void* item);

// append an element on the right side
// amortized O(1)
// creates a new FTree
FTree* FTree_appendRight(FTree* tree, void* item);

// pop an element from the left side
// amortized O(1)
// creates a new FTree
FView FTree_viewLeft(FTree* tree);

// pop an element from the left side
// amortized O(1)
// creates a new FTree and a new FView
FView* FTree_viewLeftPtr(FTree* tree);

// pop an element from the right side
// amortized O(1)
// creates a new FTree
FView FTree_viewRight(FTree* tree);

// pop an element from the right side
// amortized O(1)
// creates a new FTree and a new FView
FView* FTree_viewRightPtr(FTree* tree);

// create a tree from an array of items
// O(n)
FTree* FTree_fromArray(size_t size, void** items);

// check if iterator is at the end of iteration
// O(1)
bool FIter_empty(FIter* iter);

// get the next item from an iterator
// amortized O(1)
void* FIter_next(FIter* iter);

// create an iterator starting from the left
// O(1)
// creates a new FIter
FIter* FIter_fromTree(FTree* tree);

// convert a tree into an array
// O(n)
// creates a new void*[]
void** FTree_toArray(FTree* tree);

// get the item at an index
// O(log(min(i, n-i))
void* FTree_index(FTree* tree, size_t index);

// set the item at an index
// O(log(min(i, n-i))
// creates a new FTree
FTree* FTree_update(FTree* tree, size_t index, void* value);

// split a tree at an index,
// such that split.left contains all elements to the left of the index
// and split.right contains all elements to the right of the index
// O(log(min(i, n-i))
// creates two new FTrees
FSplit FTree_splitAt(FTree* tree, size_t index);

// version of FTree_splitAt that creates a new FSplit
// O(log(min(i, n-i))
// creates a new FSplit and two new FTrees
FSplit* FTree_splitAtPtr(FTree* tree, size_t index);

// create a new tree containing the same item repeated count times
// O(log(n))
// creates a new FTree
FTree* FTree_replicate(size_t count, void* item);

// vim: set foldmethod=marker foldlevel=0 :
