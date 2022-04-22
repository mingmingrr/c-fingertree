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
	size_t   refs;
	size_t   size;
	char     count;
	FNode*    items[4];
} FDigit;

typedef struct FTree FTree;

typedef struct FDeep {
	size_t size;
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
	size_t refs;
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
	unsigned index;
	union {
		FTree* tree;
		FNode* node;
		FDigit* digit;
	};
	struct FIterCons* next;
} FIterCons;

typedef struct FIter {
	FIterCons* stack;
} FIter;

typedef struct FView {
	void* item;
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

FTree* FTree_alloc();

FDeep* FDeep_alloc();

FDigit* FDigit_alloc();

FNode* FNode_alloc();

FIterCons* FIterCons_alloc();

FIter* FIter_alloc();

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

bool FTree_empty(FTree* tree);

size_t FTree_size(FTree* tree);

FTree* FTree_appendLeft(FTree* tree, void* item);

FTree* FTree_appendRight(FTree* tree, void* item);

FView FTree_viewLeft(FTree* tree);

FView* FTree_viewLeftPtr(FTree* tree);

FView FTree_viewRight(FTree* tree);

FView* FTree_viewRightPtr(FTree* tree);

FTree* FTree_fromArray(size_t size, void** items);

bool FIter_empty(FIter* iter);

void* FIter_next(FIter* iter);

FIter* FIter_fromTree(FTree* tree);

void** FTree_toArray(FTree* tree);

void* FTree_index(FTree* tree, size_t index);

FTree* FTree_update(FTree* tree, size_t index, void* value);

FSplit FTree_splitAt(FTree* tree, size_t index);

FSplit* FTree_splitAtPtr(FTree* tree, size_t index);

FTree* FTree_replicate(size_t count, void* item);

// vim: set foldmethod=marker foldlevel=0 :
