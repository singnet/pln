/**
 * types.h
 *
 * Copyright(c) 2001 Thiago Maia, Andre Senna
 * All rights reserved.
 */

#ifndef TYPES_H
#define TYPES_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include "platform.h"

typedef Util::ConstCharPointerIntHashMap ClassTypeHashMap;
typedef Util::IntConstCharPointerHashMap ClassNameHashMap;

// initial definition of a handle while the system is not distributed
typedef class Atom* Handle;

extern const Handle UNDEFINED_HANDLE;

//#ifdef WIN32
//typedef hash_map<Handle, void *> HandleVoidPointerHashMap;
//#else
struct hashHandle{
    int operator()(Handle h) const;
};

struct eqHandle{
    bool operator()(Handle h1, Handle h2) const;
}; 

typedef Util::hash_map<Handle, void *, hashHandle, eqHandle> HandleVoidPointerHashMap;
//#endif

// type and arity of Atoms, represented as short integers (16 bits)
typedef unsigned short Type;
typedef unsigned short Arity;

// This was a 16-bit representation of a float, which has access methods implemented by
// the ShortFloatOps class. It has changed to a float but in the future can be brought back
// to a smaller representation
typedef float ShortFloat;


/**
 * This class provides basic operations over the ShortFloat type.
 */
class ShortFloatOps {

    private:

        /**
         * Private default constructor for this class to make it abstract.
         */
        ShortFloatOps() {}

    public:

        /**
         * Maps ShortFloat to float.
         *
         * @return Float conversion of a ShortFloat.
         */
        static float getValue(const ShortFloat*);

        /**
         * Maps float to ShortFloat.
         *
         * @param ShortFloat location to receive value.
         * @param Float value to be set.
         */
        static void setValue(ShortFloat*, float);
};

// structure used to return a linked-list of atoms instead of the standard
// linked-list of handles (HandleEntry)
struct AtomEntry {
    AtomEntry *next;
    Atom *atom;
};

#endif
