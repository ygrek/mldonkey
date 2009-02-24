/* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA */
/*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/
#include <stdlib.h>
#include <string.h>

typedef char (*converter)(char c);

void* index_alloc(int size)
{
  return malloc(size);
}

void index_free(void *ptr)
{
  free(ptr);
}

typedef enum { WORD_ARRAY, WORD_TREE, WORD_HITS } tree_type;

typedef struct tree_struct tree_struct;
typedef struct word_struct word_struct;
typedef struct hits_struct hits_struct;
typedef struct array_struct array_struct;
typedef struct suffix_struct suffix_struct;
typedef struct index_struct index_struct;

struct hits_struct {
  int hits_type;
  int hits_nbr;
  int hits_words[0];
};

struct tree_struct {
  int tree_type;
  int tree_word; /* the word corresponding to the 
     current position in the tree */
  tree_struct* tree_subtrees[0];
};

typedef struct suffix_struct {
  char *suffix_word;
  int suffix_num;
};

struct array_struct {
  int array_type;
  int array_word;
  int array_nprefixes;
  int array_nsuffixes;
  suffix_struct array_suffixes[0];
};

struct word_struct {
  int word_num;
  word_struct *word_other;
  char *word_string;
};

struct index_struct {
  word_struct **dict;   /* word index --> position dans words */
  int next_word;
  int dict_len;

  tree_struct *tree;

  int range;
  converter f;
};

#define DICT_INITIAL_SIZE 16
#define WORD_INITIAL_SIZE 30000
#define MAX_LINEAR_SEARCH 20
#define MAX_WORD_SIZE     100

static char stemmed_word[MAX_WORD_SIZE];
static int word_len;


index_struct *create_index(int range, converter f)
{
  index_struct *idx = index_alloc(sizeof(index_struct));

  idx->dict = index_alloc(sizeof(word_struct*) * DICT_INITIAL_SIZE);
  idx->next_word = 0;
  idx->dict_len = DICT_INITIAL_SIZE;

  idx->tree = NULL;

  idx->range = range;
  idx->f = f;

  return idx;
}


static int add_new_word(index_struct *idx, char *word)
{
  int word_len = strlen(word);
  int w;
  int left;
  char *word_pos;
  word_struct* ws;

  if(idx->next_word == idx->dict_len){
  /* must reallocate the dictionnary */
    int old_size = idx->dict_len;
    int new_size = old_size * 2;
    word_struct** new_dict = index_alloc(sizeof(word_struct*) * new_size);

    memcpy(new_dict, idx->dict, sizeof(word_struct*) * old_size);
    index_free(idx->dict);
    idx->dict = new_dict;
    idx->dict_len = new_size;
  }
  w = idx->next_word;
  idx->next_word++;

  ws = index_alloc(sizeof(word_struct) + word_len + 1);
  idx->dict[w] = ws;
  strncpy(ws->word_string, word, word_len);
  ws->word_string[word_len] = 0;

  return w;
}


static int add_stemmed_word(index_struct *idx){
  return add_new_word(idx, stemmed_word);
}

static array_struct *alloc_array(index_struct *idx)
{
  array_struct *array=  index_alloc(sizeof(tree_struct) + 
    2 * MAX_LINEAR_SEARCH * sizeof(suffix_struct));
  array->array_type = WORD_ARRAY;
  array->array_word = -1;
  array->array_nprefixes = 0;
  array->array_nsuffixes = 0;

  return array;
}

static int add_prefix_in_tree(index_struct *idx, char *suffix, 
  tree_struct *tree, tree_struct **tree_ptr)
{
  if(tree == NULL){
    tree = (tree_struct*) alloc_array(idx);
    *tree_ptr = tree;
  }
  
  if(suffix[0] == 0) {
    if(tree->tree_word == (-1)) tree->tree_word = add_stemmed_word(idx);
    return tree->tree_word;
  }
  
  if(tree->tree_type == WORD_TREE){
    
    tree_struct * new_tree = tree->tree_subtrees[ (int) suffix[0]];
    return add_prefix_in_tree(idx, suffix+1, new_tree,
      & (tree->tree_subtrees[(int) suffix[0]]));
  
  } else {
    array_struct * array = (array_struct*) tree;
    
    int nprefixes = array->array_nprefixes;
    int suffix_len = strlen(suffix);
    int i;
    
    for(i=0; i<nprefixes; i++){
      char *word = array->array_suffixes[i].suffix_word;
      if(!strcmp(suffix, word)){
        return array->array_suffixes[i].suffix_num;
      }
    }

      /* not found */
    if(nprefixes == MAX_LINEAR_SEARCH){
      int size = idx->range * sizeof(tree_struct*);
      int nsuffixes = array->array_nsuffixes;        
      tree_struct *new_tree = index_alloc(sizeof(tree_struct) + size);
      
      bzero(new_tree->tree_subtrees, size);
      new_tree->tree_type = WORD_TREE;
      new_tree->tree_word = -1;
      
      *tree_ptr = new_tree;
      
      for(i=0; i<nprefixes; i++){
        char *word = array->array_suffixes[i].suffix_word;
        int num = array->array_suffixes[i].suffix_num;

        tree_struct *next_tree = new_tree->tree_subtrees[(int)word[0]];
        array_struct *nr = (array_struct*) next_tree;
        
        if(nr == NULL){
          nr = alloc_array(idx);
          new_tree->tree_subtrees[(int) word[0]] = (tree_struct*) nr;
        }
        nr->array_suffixes[nr->array_nprefixes].suffix_word = word+1;
        nr->array_suffixes[nr->array_nprefixes].suffix_num = num;
        nr->array_nprefixes++;
      }
      
      for(i=0; i<nsuffixes; i++){
        char *word = array->array_suffixes[MAX_LINEAR_SEARCH+i].suffix_word;
        int num = array->array_suffixes[i].suffix_num;

        tree_struct *next_tree = new_tree->tree_subtrees[(int)word[0]];
        array_struct *nr = (array_struct*) next_tree;
        
        if(nr == NULL){
          nr = alloc_array(idx);
          new_tree->tree_subtrees[(int) word[0]] = (tree_struct*) nr;
        }
        nr->array_suffixes[MAX_LINEAR_SEARCH+nr->array_nsuffixes].suffix_word = word+1;
        nr->array_suffixes[MAX_LINEAR_SEARCH+nr->array_nsuffixes].suffix_num = num;
        nr->array_nsuffixes++;
      }
      
      index_free(array);
      return add_prefix_in_tree(idx, suffix+1, 
        new_tree->tree_subtrees[(int) suffix[0]],
        & new_tree->tree_subtrees[(int) suffix[0]]);
    } else {
      int w = add_stemmed_word(idx);
      array->array_nprefixes = nprefixes+1;
      array->array_suffixes[nprefixes].suffix_num = w;
      array->array_suffixes[nprefixes].suffix_word =
      idx->dict[w]->word_string + word_len - suffix_len;
      return w;
    }
  }
}


int add_word(index_struct *idx, char *word)
{
  word_len = strlen(word);
  int i;
  int w;
  int ww;

  for(i = 0; i < word_len; i++)
    stemmed_word[i] = idx->f(word[i]);
  stemmed_word[word_len] = 0;
  
  w = add_prefix_in_tree(idx, stemmed_word,  idx->tree,& idx->tree);
  if(!strcmp(stemmed_word, word)) return w;

  /* the stemmed word is not the current word */
  ww = add_new_word(idx, word);

  idx->dict[ww]->word_other = idx->dict[w]->word_other;  
  idx->dict[w]->word_other = idx->dict[w];

  return ww;
}

/*
"toto" ---> 464 --> a,b,c,...
*/

char *get_word(index_struct *idx, int w)
{
  return idx->dict[w]->word_string;
}

static int nhits;
static int hits[MAX_LINEAR_SEARCH];

static void add_hit(int num)
{
  hits[nhits++] = num;
}

static tree_struct*
find_in_tree(index_struct *idx, char *suffix, tree_struct *tree)
{
  if(tree == NULL) return NULL;
  
  if(suffix[0] == 0) return tree;
  
  if(tree->tree_type == WORD_TREE){
    
    tree_struct * new_tree = tree->tree_subtrees[ (int) suffix[0]];
    return find_in_tree(idx, suffix+1, new_tree);
  
  } else {
    array_struct * array = (array_struct*) tree;
    
    int nprefixes = array->array_nprefixes;
    int nsuffixes = array->array_nsuffixes;
    int suffix_len = strlen(suffix);
    int i;
    hits_struct *all_hits;    

    nhits = 0;
    
    for(i=0; i<nprefixes; i++){
      char *word = array->array_suffixes[i].suffix_word;
      if(!strncmp(suffix, word, suffix_len)){
        add_hit(array->array_suffixes[i].suffix_num);
      }
    }
    for(i=0; i<nsuffixes; i++){
      char *word = array->array_suffixes[MAX_LINEAR_SEARCH+i].suffix_word;
      if(!strncmp(suffix, word, suffix_len)){
        add_hit(array->array_suffixes[MAX_LINEAR_SEARCH+i].suffix_num);
      }
    }
    if(nhits == 0) return NULL;
    all_hits = index_alloc(sizeof(hits_struct) + sizeof(int) * nhits);
    all_hits->hits_type = WORD_HITS;
    all_hits->hits_nbr = nhits;
    memcpy(all_hits->hits_words, hits, nhits * sizeof(int));
    return (tree_struct*) all_hits;
  }
}


tree_struct *find_word(index_struct *idx, char *word)
{
  word_len = strlen(word);
  int i;

  for(i = 0; i < word_len; i++)
    stemmed_word[i] = idx->f(word[i]);
  stemmed_word[word_len] = 0;
  
  return find_in_tree(idx, stemmed_word,  idx->tree);
}

