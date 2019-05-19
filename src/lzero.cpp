#include <assert.h>
#include <map>
#include <stack>
#include <vector>
using namespace std;

struct Hix {
    int hix;

    Hix() : hix(-42){};

    Hix(int hix) : hix(hix){};
    // int operator ()() const {
    //     return hix;
    // }

    bool operator==(const Hix &other) const { return hix == other.hix; }

    bool operator!=(const Hix &other) { return hix != other.hix; }

    // provided for convenience to bump up the value
    void operator+=(int i) { hix += i; }

    // operator provided for convenience to index heap values that come
    // after the current index
    Hix operator+(int i) const { return hix + i; }

    // for std::map
    bool operator<(const Hix &other) const { return hix < other.hix; }
};

struct Register {
    int r;
    Register() : Register(-42) {}

    Register(int r) : r(r){};

    bool operator==(const Register &other) const { return r == other.r; }

    bool operator!=(const Register &other) { return r != other.r; }

    // for std::map
    bool operator<(const Register &other) const { return r < other.r; }
};

enum class Htag { Ref, Str, Functor };

struct Functor {
    int id;
    int arity;

    bool operator == (const Functor &other) const {
        return id == other.id && arity == other.arity;
    }
};

struct Hcell {
    Htag tag;
    Hix hix;
    Functor f;

    static Hcell str(Hix hix) {
        Hcell h;
        h.tag = Htag::Str;
        h.hix = hix;
        return h;
    }

    static Hcell ref(Hix hix) {
        Hcell h;
        h.tag = Htag::Ref;
        h.hix = hix;
        return h;
    }

    static Hcell functor(Functor f) {
        Hcell h;
        h.tag = Htag::Functor;
        h.f = f;
        return h;
    }

    bool operator == (const Hcell &other) const {
        if (tag != other.tag) return false;
        switch (tag) {
            case Htag::Ref:
            case Htag::Str:
                return hix == other.hix;
            case Htag::Functor:
                return f == other.f;
        }
        assert(false && "unreachable");
    }
};

enum class AddrTag { Reg, Heap };
// TODO: how to overload casting?
struct Addr {
   private:
    AddrTag tag_;
    Register r_;
    Hix hix_;

   public:
    Addr(Register r) : tag_(AddrTag::Reg), r_(r) {}
    Addr(Hix hix) : tag_(AddrTag::Heap), hix_(hix) {}

    Hix hix() const {
        assert(tag_ == AddrTag::Heap);
        return hix_;
    }

    Register r() const {
        assert(tag_ == AddrTag::Reg);
        return r_;
    }

    AddrTag tag() const { return tag_; }

    bool operator==(const Addr &other) const {
        if (other.tag_ != tag_) return false;

        switch (tag_) {
            case AddrTag::Heap:
                return hix_ == other.hix_;
            case AddrTag::Reg:
                return r_ == other.r_;
        }
        assert(false && "unreachable");
    }

    bool operator!=(const Addr &other) const { return !(*this == other); }
};

enum class Mode { Read, Write };
struct Machine {
    map<Hix, Hcell> heap;
    map<Register, Hcell> regval;
    Hix h;
    Hix s;
    Mode mode;
    stack<Addr> pdl;
};

// Figure 2.2
Machine putStructure(Functor f, Register r, Machine m) {
    m.regval[r] = m.heap[m.h] = Hcell::str(m.h + 1);
    m.heap[m.h + 1] = Hcell::functor(f);
    m.h += 2;
    return m;
}

// Figure 2.2
Machine setVariale(Register r, Machine m) {
    m.regval[r] = m.heap[m.h] = Hcell::ref(m.h);
    m.h += 1;
    return m;
}

// Figure 2.2
Machine setValue(Register r, Machine m) {
    m.heap[m.h] = m.regval[r];
    m.h += 1;
    return m;
}

// index the machine at a general address
// In the book, this is written as STORE[addr]
Hcell machineAtAddr(Machine m, Addr a) {
    switch (a.tag()) {
        case AddrTag::Reg:
            return m.regval[a.r()];
        case AddrTag::Heap:
            return m.heap[a.hix()];
    }
    assert(false && "unreachable");
}

// figure 2.5
Addr deref(Machine m, Addr a) {
    const Hcell hc = machineAtAddr(m, a);
    // if we have a reference to chase which is not pointing to itself, then
    // chase it
    if (hc.tag == Htag::Ref && Addr(hc.hix) != a) {
        deref(m, hc.hix);
    }
    return a;
}


// bind: page 17
// In the definition of get structure f=n X i , we write bind(addr, H) 
// to effectuate the binding of the heap cell rather than HEAP[addr] <- <REF, H>
// for reasons that will become clear later.
// This is slightly generalized from the version they give for the
// second argument to be an address so we can use it the way they do
// in (2.7: the unify operation)
Machine bind(Machine m, Addr storeLoc, Addr toStore) {
    m.heap[storeLoc.hix()] = Hcell::ref(toStore.hix());
    return m;
};

// Figure 2.6
Machine getStructure(Functor f, Register r, Machine m) {
    const Addr addr = deref(m, r);
    const Hcell hc = machineAtAddr(m, addr);

    switch(hc.tag) {
        case Htag::Ref:
            m.heap[m.h] = Hcell::ref(m.h + 1);
            m.heap[m.h+1] = Hcell::functor(f);
            m = bind(m, addr, m.h);
            m.h += 2;
            m.mode = Mode::Write;
            break;
        case Htag::Str:
            if (m.heap[hc.hix] == Hcell::functor(f)) {
                m.s += 1;
                m.mode = Mode::Read;
            }
            break;
        case Htag::Functor:
            assert(false && "cannot call getStructure on a functor");
    }
    return m;
}

// Figure 2.6
Machine unifyVariable(Machine m, Register r) {
    switch (m.mode) {
        case Mode::Read:
            m.regval[r] = m.heap[m.s];
            break;
        case Mode::Write:
            m.heap[m.h] = Hcell::ref(m.h);
            m.regval[r] = m.heap[m.h];
            m.h += 1;
            break;
    }
    m.s += 1;
    return m;
}

// Figure 2.7
// It is written needlessly in an imperative style using a stack.
// With recursion, we can elide the need for the intermediate PDL strcture.
// Of course, recursion has its own problems, but as far as I can tell,
// we will not be running programs that are large enough to blow our
// system call stack.
Machine unify(Machine m, Addr a1, Addr a2) {
    Addr d1 = deref(m, a1), d2 = deref(m, a2);
    if (d1 == d2) return m;

    Hcell h1 = machineAtAddr(m, d1), h2 = machineAtAddr(m, d2);

    if (h1.tag == Htag::Ref || h2.tag == Htag::Ref) {
        return bind(m, d1, d2);
    }
    else {
        Functor f1 = m.heap[h1.hix].f, f2 = m.heap[h2.hix].f;
        assert(f1 == f2);

        for(int i = 1; i <= h1.f.arity; i++) {
            // We now union the subterms of the functor
            m = unify(m, h1.hix + i, h2.hix + i);
        }
    }

    return m;
};


//Figure 2.6
Machine unifyValue(Machine m, Register r) {
    switch (m.mode) {
        case Mode::Read:
            m = unify(m, r, m.s);
            break;
        case Mode::Write:
            m.heap[m.h] = m.regval[r];
            m.h += 1;
            break;
    }
    m.s += 1;
    return m;
}
