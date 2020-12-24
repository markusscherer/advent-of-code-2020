#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

struct List {
  uint32_t v;
  struct List * next;
  struct List * target;
};

void print(struct List* l) {
  struct List* head = l;
  struct List* cur = head->next;
  printf("%d ", head->v);
  int i = 0;
  while(cur != head && i++ < 10) {
    printf("%d ", cur->v);
    cur = cur->next;
  }
  printf("\n");
}

void print1(struct List* l) {
  struct List* cur = l;
  while(cur->v != 1) {
    cur = cur->next;
  }
  print(cur);
  uint64_t res = (uint64_t) cur->next->v * (uint64_t) cur->next->next->v;

  printf("%ld\n", res);
}

struct List* findTarget(struct List* l) {
  struct List* head = l;
  struct List* cur = head->next;
  uint32_t inf = 0;
  uint32_t max = 0;
  uint32_t t = head->v;
  struct List* infp = NULL;
  struct List* maxp = NULL;

  while(cur != head) {
    if(cur->v == t - 1) {
      return cur;
    }
    if(cur->v > t && cur->v > max) {
      max = cur->v;
      maxp = cur;
    }
    if(cur->v < t && cur->v > inf) {
      inf = cur->v;
      infp = cur;
    }
    cur = cur->next;
  }

  if(inf != 0) {
    return infp;
  }
  return maxp;
}

void shuffleRound(struct List* l) {
  struct List* head = l;
  struct List* pickup1 = head->next;
  struct List* pickup2 = pickup1->next;
  struct List* pickup3 = pickup2->next;

  struct List* target = head->target;

  while (target == pickup1 || target == pickup2 || target == pickup3) {
    target = target->target;
  }

  head->next = pickup3->next;
  pickup3->next = target->next;
  target->next = pickup1;
}

void shuffleRounds(struct List* l, int n) {
  struct List* next = l;
  for(int i = 0; i < n; ++i) {
    shuffleRound(next);
    next = next->next;
  }
}

int main() {
  /* int COUNT = 9; */
  int COUNT = 1000000;
  struct List *p = malloc(sizeof(struct List) * COUNT);

// test initialization
//  p[0].v = 3;
//  p[1].v = 8;
//  p[2].v = 9;
//  p[3].v = 1;
//  p[4].v = 2;
//  p[5].v = 5;
//  p[6].v = 4;
//  p[7].v = 6;
//  p[8].v = 7;

  p[0].v = 3;
  p[1].v = 8;
  p[2].v = 9;
  p[3].v = 5;
  p[4].v = 4;
  p[5].v = 7;
  p[6].v = 6;
  p[7].v = 1;
  p[8].v = 2;

  for(int i = 9; i < COUNT; ++i) {
    p[i].v = i+1;
    p[i].target = &p[i-1];
  }

  for(int i = 0; i < COUNT; ++i) {
    p[i].next = &p[i+1];
  }
  p[COUNT-1].next = &p[0];

  for(int i = 0; i < 9+1; ++i) {
    p[i].target = findTarget(&p[i]);
  }

  shuffleRounds(&p[0], 10000000);

  print1(&p[0]);
}
