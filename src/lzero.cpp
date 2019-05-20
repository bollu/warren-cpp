#include <assert.h>
#include <iostream>
#include <map>
#include <set>
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

    void print(std::ostream &o, bool color = false) const { o << "0x" << hix; }

};

std::ostream &operator<<(std::ostream &o, Hix hix) {
    hix.print(o);
    return o;
}

struct Register {
    int r;
    Register() : Register(-42) {}

    Register(int r) : r(r){};

    bool operator==(const Register &other) const { return r == other.r; }

    bool operator!=(const Register &other) { return r != other.r; }

    // for std::map
    bool operator<(const Register &other) const { return r < other.r; }

    void print(std::ostream &o, bool color = false) const { o << "%" << r; }

};

std::ostream &operator<<(std::ostream &o, Register r) {
    r.print(o);
    return o;
}

enum class Htag { Ref, Str, Functor };

struct Functor {
    string id;
    int arity;

    Functor() : id("garbage"), arity(-42){};
    Functor(string id, int arity) : id(id), arity(arity){};
    bool operator==(const Functor &other) const {
        return id == other.id && arity == other.arity;
    }
    void print(ostream &o) {
        o << id << "/" << arity;
    }
};

std::ostream &operator<<(std::ostream &o, Functor f) {
    f.print(o);
    return o;
}

struct Hcell {
    Htag tag;
    Hix hix_;
    Functor f_;

    static Hcell str(Hix hix) {
        Hcell h;
        h.tag = Htag::Str;
        h.hix_ = hix;
        return h;
    }

    static Hcell ref(Hix hix) {
        Hcell h;
        h.tag = Htag::Ref;
        h.hix_ = hix;
        return h;
    }

    static Hcell functor(Functor f) {
        Hcell h;
        h.tag = Htag::Functor;
        h.f_ = f;
        return h;
    }

    bool operator==(const Hcell &other) const {
        if (tag != other.tag) return false;
        switch (tag) {
            case Htag::Ref:
            case Htag::Str:
                return hix_ == other.hix_;
            case Htag::Functor:
                return f_ == other.f_;
        }
        assert(false && "unreachable");
    }

    Hix hix() const {
        assert(tag == Htag::Ref || tag == Htag::Str);
        return hix_;
    }

    // This seems dangerous
    // operator Hix () const {
    //     return hix();
    // }

    // This is less dangerous, and gives us a way to "destructure" nicely.
    operator Functor() const { return f(); }

    Functor f() const {
        assert(tag == Htag::Functor);
        return f_;
    }

    void print(ostream &o) {
        switch(tag) {
            case Htag::Ref:
                o << "REF(" << hix_ <<")";
                return;
            case Htag::Str:
                o << "STR(" << hix_ <<")";
                return;
            case Htag::Functor:
                o << "F(" << f_ <<")";
        }
    }
};


std::ostream &operator<<(std::ostream &o, Hcell hc) {
    hc.print(o);
    return o;
}

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

    Machine()  : h(0), s(0), mode(Mode::Read) {};
};

// Figure 2.2
Machine putStructure(Machine m, Register r, Functor f) {
    m.regval[r] = m.heap[m.h] = Hcell::str(m.h + 1);
    m.heap[m.h + 1] = Hcell::functor(f);
    m.h += 2;
    return m;
}

// Figure 2.2
Machine setVariable(Machine m, Register r) {
    m.regval[r] = m.heap[m.h] = Hcell::ref(m.h);
    m.h += 1;
    return m;
}

// Figure 2.2
Machine setValue(Machine m, Register r) {
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
    if (hc.tag == Htag::Ref && Addr(hc.hix()) != a) {
        deref(m, hc.hix());
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
Machine getStructure(Machine m, Register r, Functor f) {
    const Addr addr = deref(m, r);
    const Hcell hc = machineAtAddr(m, addr);

    switch (hc.tag) {
        case Htag::Ref:
            m.heap[m.h] = Hcell::ref(m.h + 1);
            m.heap[m.h + 1] = Hcell::functor(f);
            m = bind(m, addr, m.h);
            m.h += 2;
            m.mode = Mode::Write;
            break;
        case Htag::Str:
            if (m.heap[hc.hix()] == Hcell::functor(f)) {
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
    } else {
        Functor f1 = m.heap[h1.hix()], f2 = m.heap[h2.hix()];
        assert(f1 == f2);

        for (int i = 1; i <= f1.arity; i++) {
            // We now union the subterms of the functor
            m = unify(m, h1.hix() + i, h2.hix() + i);
        }
    }

    return m;
};

// Figure 2.6
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

// value that are available first order terms.
// A variable is a captialized identifier
// A functor is a structure of the form f(t1, t2, .. tn) where the
//     t1 .. tn are are the sub-terms.
// A constant is a functor with 0 arguments.
enum class FOTermTag { Variable, Functor };

// First order terms
struct FOTerm {
    std::vector<FOTerm> params;  // parameters if it's a functor.
    FOTermTag tag;
    std::string name;  // name of a variable, constant, or functor.

    static FOTerm variable(std::string vname) {
        FOTerm t;
        t.tag = FOTermTag::Variable;
        // should be capital
        assert(vname[0] >= 'A' && vname[0] <= 'Z');
        t.name = vname;
        return t;
    }

    static FOTerm functor(std::string fname,
                          std::initializer_list<FOTerm> params) {
        FOTerm t;
        // should be small letter
        assert(fname[0] >= 'a' && fname[0] <= 'z');
        t.name = fname;
        t.tag = FOTermTag::Functor;
        t.params = params;
        return t;
    }

    static FOTerm constant(std::string name) { return functor(name, {}); }

    void print(ostream &o) {
        switch (tag) {
            case FOTermTag::Variable:
                o << name;
                return;
            case FOTermTag::Functor:
                o << name <<"(";
                for(int i =0; i < params.size();++i) {
                    params[i].print(o);
                    if (i + 1 < params.size())
                        o << ", ";
                }
                o << ")";

        }
    }
};


std::ostream &operator<<(std::ostream &o, FOTerm t) {
    t.print(o);
    return o;
}

// flattened first order term
// Note that this can be unioned with FOTTerm if FOTTerm were made
// a template over what it stores.
struct FOTermFlattened {
    std::vector<Register> params;
    FOTermTag tag;
    std::string name;

    static FOTermFlattened variable(std::string vname) {
        FOTermFlattened t;
        t.tag = FOTermTag::Variable;
        // should be capital
        assert(vname[0] >= 'A' && vname[0] <= 'Z');
        t.name = vname;
        return t;
    }


    static FOTermFlattened functor(std::string fname,
                          std::vector<Register> params) {
        FOTermFlattened t;
        // should be small letter
        assert(fname[0] >= 'a' && fname[0] <= 'z');
        t.name = fname;
        t.tag = FOTermTag::Functor;
        t.params = params;
        return t;
    }

    void print(ostream &o) {
        switch (tag) {
            case FOTermTag::Variable:
                o << name;
                return;
            case FOTermTag::Functor:
                o << name <<"(";
                for(int i =0; i < params.size();++i) {
                    params[i].print(o);
                    if (i + 1 < params.size())
                        o << ", ";
                }
                o << ")";

        }
    }
};


std::ostream &operator<<(std::ostream &o, FOTermFlattened t) {
    t.print(o);
    return o;
}

// the state of the Flattener which prepares the initial state of the
// machine.
struct Flattener {
    Machine m;
    std::map<std::string, Register> var2reg;
    // The flattened system of equations
    std::map<Register, FOTermFlattened> flat;
};

// flatten a FOTerm into a system of FOTermFlattened
Register flatten(Flattener &c, FOTerm term) {
    switch (term.tag) {
        case FOTermTag::Variable: {
            auto it = c.var2reg.find(term.name);
            // variable has already been allocated, nothing more to do
            if (it != c.var2reg.end()) return it->second;

            // allocate a variable. So, populate the flattened representation,
            // and create a new mapping from the variable to the register.
            Register rnew = c.flat.size() + 1;
            c.flat[rnew] = FOTermFlattened::variable(term.name);
            c.var2reg[term.name] = rnew;
            return rnew;
        }

        case FOTermTag::Functor: {
            // create a new register for the functor
            // register for parameters
            std::vector<Register> prs;
            Register rnew = c.flat.size() + 1;
            // HACK: reserve this register.
            c.flat[rnew];
            for (auto it : term.params) {
                prs.push_back(flatten(c, it));
            }
            c.flat[rnew] = FOTermFlattened::functor(term.name, prs);
            return rnew;
        }
    }
    assert(false && "unreachable");
};

// compile a flattened system of queries into a machine state
Machine compileQuery(Machine m, std::map<Register, FOTermFlattened> flat) {
    // checks if a register has been seen before.
    std::set<Register> seenregs;
    for (auto it = flat.begin(); it != flat.end(); ++it) {
        switch (it->second.tag) {
            case FOTermTag::Functor: {
                Functor f = Functor(it->second.name, it->second.params.size());
                m = putStructure(m, it->first, f);
            }
            case FOTermTag::Variable: {
                // we've seen this variable
                if (seenregs.count(it->first)) {
                    m = setValue(m, it->first);
                } else {
                    m = setVariable(m, it->first);
                    seenregs.insert(it->first);
                }
            }
        }
    }
    return m;
};

// compile a flattened system of program expression into a machine state
// compare to compileQuery, notice how similar the code is
// TODO: extract out the common parts into a higher order function.
Machine compileProgram(Machine m, std::map<Register, FOTermFlattened> flat) {
    return m;
    // checks if a register has been seen before.
    std::set<Register> seenregs;
    for (auto it = flat.begin(); it != flat.end(); ++it) {
        switch (it->second.tag) {
            case FOTermTag::Functor: {
                Functor f = Functor(it->second.name, it->second.params.size());
                m = getStructure(m, it->first, f);
            }
            case FOTermTag::Variable: {
                // we've seen this variable
                if (seenregs.count(it->first)) {
                    m = unifyValue(m, it->first);
                } else {
                    m = unifyVariable(m, it->first);
                    seenregs.insert(it->first);
                }
            }
        }
    }
    return m;
};

void prettyPrintMachine(Machine m) {
    // H, S
    cout << "H: " << m.h << "|S: " << m.s << "\n";

    cout << "Heap:\n";
    for(auto it: m.heap) {
        cout << it.first << " -> " << it.second;
        cout << "\n";
    }
    cout << "\n--\n";

    cout << "Registers:\n";
    for(auto it: m.regval) {
        cout << it.first << " -> " << it.second;
        cout << "\n";
    }
    cout << "\n--\n";
}

template<typename K, typename V>
void printMap(std::ostream &o, const std::map<K, V> &m) {
    for(auto it: m) {
        o << it.first << " -> " << it.second << "\n";
    }
}

// pretty print to ensure that we get a similar heap representation as in
// figure 2.1
void fig21() {
    FOTerm W = FOTerm::variable("W");
    FOTerm Z = FOTerm::variable("Z");
    FOTerm fW = FOTerm::functor("f", {W});
    FOTerm hZW = FOTerm::functor("h", {Z, W});
    FOTerm p = FOTerm::functor("p", {Z, hZW, fW});

    cout << "*** flattening term: " << p << " ***\n";

    Flattener f;
    (void)flatten(f, p);

    cout << "*** flattened representation (page 15) ***\n";
    printMap(std::cout, f.flat);

    Machine m;
    m = compileQuery(m, f.flat);

    cout << "*** machine state after query compilation ***\n";
    prettyPrintMachine(m);
}
