#include <map>
#include <vector>
using namespace std;

using Hix = int;
// register type
using Register = int;

enum class HTag { Ref, Str, Functor };

struct Functor {
    int id;
    int arity;
};

class Hcell {
    HTag tag;
    Hix hix;
    Functor f;

    static HCell str(Hix hix) {
        HCell h;
        h.tag = HTag.Str;
        h.hix = hix;
        return h;
    }

    static HCell ref(Hix hix) {
        HCell h;
        h.tag = HTag.Ref;
        h.hix = hix;
        return h;
    }

    static HCell functor(Functor f) {
        HCell h;
        h.tag = HTag.Functor;
        h.f = f;
        return h;
    }
};

enum class AddrTag { Reg, Heap };
struct Addr {
   private:
    AddrTag tag;
    union {
        Reg r;
        Hix hix;
    } val;

   public:
    Addr(Reg r) {
        tag = AddrTag.Reg;
        val.r = r;
    }

    Addr(Hix hix) {
        tag = AddrTag.Heap;
        val.hix = hix;
    }

    Hix hix() const {
        assert(tag == AddrTag.Heap);
        return val.hix;
    }

    Reg r() const {
        assert(tag == AddrTag.Reg);
        return val.r;
    }

    AddrTag tag() const { return tag; }
};

enum class Mode { MR, MW };
struct Machine {
    map<Hix, Hcell> heap;
    map<Reg, Hcell> regval;
    Hix h;
    Hix s;
    Mode mode;
    stack<Addr> pdl;
};

Machine putStructure(Functor f, Register r, Machine m) { 
    m.regval[r] = m.heap[m.h] = Hcell::str(m.h + 1);
    m.heap[m.h + 1] = Hcell::functor(f);
    m.h += 2;
    return m;
}

Machine setVariale(Register r, Machine m) {
    m.regval[r] = m.heap[m.h] = HCell::ref(m.h);
    m.h += 1;
}

Machine setValue(Register r, Machine m) {
    m.heap[m.h] = m.regval[r];
    m.h += 1;
}

Addr deref(Addr r) {
    // todo: implement
}
Machine getStructure(Functor f, Register r, Machine m) {
}
