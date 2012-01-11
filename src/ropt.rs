//  Copyright © 2012 bjarneh
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.


//////////////////////////////////////////////////////////////////
//
// Just an alternative 'getopt' implementation to check out
// the language rust. See usage from main function at the bottom
// of the file.
// 
//////////////////////////////////////////////////////////////////

use std;
import std::io::{ println };

mod ropt{

    import std::io::{ println,
                      print };
    import std::vec::{ len,
                       is_not_empty };
    import std::str::{ byte_len,
                       char_len,
                       to_chars,
                       from_char,
                       substr,
                       split,
                       connect,
                       slice,
                       starts_with };
    import std::map::{ hashmap,
                       hashset,
                       mk_hashmap,
                       new_str_hash };
    import std::sort::{ merge_sort };


    type parser = obj {
        fn add_bool(s: str);
        fn add_str(s: str);
        fn add_str_fancy(s: str);
        fn is_set(s: str) -> bool;
        fn get(s: str) -> str;
        fn juxta_bool(s: str) -> bool;
        fn juxta_str(s: str) -> bool;
        fn get_many(s: str) -> [str];
        fn opts() -> hashmap<str,option>;
        fn parse(argv: [str]) -> [str];
        fn short_set_options() -> [str];
        fn reset();
    };

    fn new_parser() -> parser {

        obj parser(options: hashmap<str,option>){

            fn add_bool(s: str) {

                let m = self.opts();
                let o = new_bool_option(s);

                for f in o.flags() {
                    m.insert(f, o);
                }
            }

            fn add_str(s: str) {

                let m = self.opts();
                let o = new_str_option(s);

                for f in o.flags() {
                    m.insert(f, o);
                }
            }

            fn add_str_fancy(s: str) {

                let m = self.opts();
                let o = new_str_option_fancy(s);

                for f in o.flags() {
                    m.insert(f, o);
                }
            }

            fn is_set(s: str) -> bool{
                let m = self.opts();
                if m.contains_key(s) {
                    let o = m.get(s);
                    ret o.is_set();
                }
                ret false;
            }

            fn get(s: str) -> str {
                let m = self.opts();
                let o = m.get(s);
                if ! o.is_set() {
                    fail("option was not set");
                }
                let ty = o.my_type();
                check( is_str_option(ty) );
                let _state = o.state();
                ret (*_state).str_state[0];
            }

            fn get_many(s: str) -> [str] {
                let m = self.opts();
                let o = m.get(s);
                if ! o.is_set() {
                    fail("option was not set");
                }
                let ty = o.my_type();
                check( is_str_option(ty) );
                let _state = o.state();
                ret (*_state).str_state;
            }

            fn opts() -> hashmap<str,option> {
                ret options;
            }

            fn parse(argv: [str]):is_not_empty(argv) -> [str] {

                let i: uint = 0u;
                let max: uint = len(argv);
                let op: option;
                let r: [str] = [];
                let m  = self.opts();

                while i < max {

                    if m.contains_key(argv[i]) {

                        op = m.get(argv[i]);

                        if op.my_type() == bool_option {
                            op.set();
                        }else{
                            if i+1u >= max {
                                println(#fmt("missing argument: %s", argv[i]));
                                fail;
                            }else{
                                i +=1u;
                                op.add(argv[i]);
                            }
                        }

                    // complex test? for both ways to juxtaposition an 
                    // option and argument i.e. '-hv' for bool, and
                    // '-I/usr/include' and so on for str options.
                    }else if !self.juxta_str(argv[i]) & !self.juxta_bool(argv[i]){

                        // if no juxtaposition is going on, we may add
                        // this argument to our remaining arguments

                        r += [argv[i]];
                    }

                    i += 1u;
                }

                ret r;
            }

            // '-I/usr/include' -> true if '-I' is str option
            fn juxta_str(s: str) -> bool {

                let o: option;
                let m = self.opts();
                let max_len: uint = 0u;
                let max_opt: str = "";

                m.keys(){ |x| 
                    if starts_with(s, x) {
                        if char_len(x) > max_len {
                            let tmp = m.get(x);
                            if tmp.my_type() == str_option {
                                max_len = char_len(x);
                                max_opt = x;
                            }
                        }
                    }
                };

                if max_len > 0u {
                    let stop = byte_len(s) - byte_len(max_opt);
                    let rest = substr(s, byte_len(max_opt), stop);
                    o = m.get(max_opt);
                    o.add(rest);
                    ret true;
                }

                ret false;
            }

            // '-xzv' -> true iff '-x', '-z' and '-v' are bool options
            fn juxta_bool(s: str) -> bool {

                let o: option;
                let m = self.opts();

                if char_len(s) > 2u && starts_with(s, "-") {

                    let possible = juxta_split(s);

                    for e in possible {

                        if m.contains_key(e) {

                            o = m.get(e);
                            if o.my_type() != bool_option {
                                ret false;
                            }

                        }else{
                            ret false;
                        }
                    }

                    // nothing but boolean options
                    for e in possible {
                        o = m.get(e);
                        o.set();
                    }
                    ret true;
                }else{
                   ret false;
                }
            }

            fn reset() {
                let m = self.opts();
                m.values(){|x|
                    x.reset();
                };
            }

            fn short_set_options() -> [str] {

                let set_options:[str] = [];
                let m = self.opts();
                let hset:hashset<str> = mk_hashmap(std::str::hash,
                                                   std::str::eq);
                m.values(){|x|
                    if x.is_set() {
                        std::map::set_add(hset,x.shortest_flag());
                    }
                };

                hset.keys(){|x|
                    set_options += [x];
                };
                ret set_options;
            }
        }
        
        let hash = new_str_hash();

        ret parser(hash);
    }

    /*

    options are either str or bool options,
    bool options are just flags which can be
    sat; they are all false prior to parsing,
    and any bool option or flag given will be
    true after parsing..

    str options take arguments, if more than
    one argument is to be given, it must be
    given multiple times, i.e.

    -I/usr/include -I/usr/local/include

    will include '/usr/include' and '/usr/local/include'
    as arguments for the '-I' str option, but this
    will not work:

    -I/usr/include /usr/local/include
    */

    const bool_option : int = 1;
    const str_option  : int = 2;

    pure fn is_str_option(i: int) -> bool {
        ret i == str_option;
    }

    type optstate = {
        mutable bool_state: bool,
        mutable str_state : [str]
    };

    type option = obj {
        fn my_type() -> int;
        fn set();
        fn is_set() -> bool;
        fn state() -> @optstate;
        fn flags() -> [str];
        fn shortest_flag() -> str;
        fn add(s: str);
        fn to_stdout();
        fn reset();
    };

    fn new_option(flags: str, kind: int) -> option {

        obj option(f: [str], t: int, st: @optstate){

            fn my_type() -> int { ret t; }

            fn set() {
                let _state = self.state();
                (*_state).bool_state = true;
            }

            fn is_set() -> bool {
                let _st = self.state();
                if self.my_type() == bool_option {
                    ret (*_st).bool_state;
                }else{
                    ret len(_st.str_state) > 0u;
                }
            }

            fn state() -> @optstate { ret st; }

            fn flags() -> [str] { ret f; }

            fn shortest_flag() -> str {
                let lteq = fn(&&a:str,&&b:str)->bool{
                    ret char_len(a) <= char_len(b);
                };
                let short_sort = merge_sort(lteq, self.flags());
                ret short_sort[0];
            }

            fn add(s: str) {
                let ty = self.my_type();
                check( is_str_option( ty ) );
                let _state = self.state();
                (*_state).str_state += [s];
            }

            fn to_stdout(){
                print(self.my_type() == bool_option? "[bool] ": "[str]  ");
                for fl in self.flags() {
                    print(fl+ " ");
                }
                print(#fmt(": %b ", self.is_set()));

                if self.my_type() == str_option {
                    print(": [ ");
                    let _st = self.state();
                    for e in (*_st).str_state {
                        print(#fmt("%s ",e));
                    }
                    print("]");
                }
                println("");
            }

            fn reset() {
                let _state = self.state();
                (*_state).bool_state = false;
                (*_state).str_state  = [];
            }
        }

        let flagvec = split(flags, ' ' as u8);
        assert len(flagvec) > 0 as uint;
        let init_st: optstate = {mutable bool_state: false,
                                 mutable str_state: []};
        ret option(flagvec, kind, @init_st);

    }

    fn new_str_option(flags: str) -> option {
        ret new_option(flags, str_option);
    }

    fn new_str_option_fancy(flags: str) -> option {
        ret new_option(fancy_options(flags), str_option);
    }

    // '-f --file' -> '-f -f= -file -file= --file --file='
    fn fancy_options(s: str) -> str {

        let op : [str] = split(s, ' ' as u8);
        let n  : [str] = [];

        for o in op {
            if starts_with(o, "--") {
                n += [ slice(o, 1u, byte_len(o)) ];
                n += [ slice(o, 1u, byte_len(o)) + "=" ];
                n += [ o + "=" ];
            }else if starts_with(o, "-") {
                n += [ o + "=" ];
            }
            n += [o];
        }

        ret connect(n, " ");
    }

    fn new_bool_option(flags: str) -> option {
        ret new_option(flags, bool_option);
    }

    // '-xzv' -> [ '-x', '-z', '-v' ]
    fn juxta_split(s: str) -> [str] {

        let ops: [str] = [];
        let ch = to_chars(slice(s, 1u, char_len(s)));
        let max: int = len(ch) as int;
        let combo, single: str;
        let i = 0;

        while i < max {
            single = from_char(ch[i]);
            combo  = "-" + single;
            ops    += [combo];
            i += 1;
        }

        ret ops;
    }

    /* unit testing */

    #[test]
    fn test_ropt() {

        let p = new_parser();

        // bool options
        p.add_bool("-h -help --help");
        p.add_bool("-v -version --version");
        // str options
        p.add_str("-I");
        p.add_str_fancy("-f --file");

        let argv: [str] = split("a1 -I/inc -I /inc2 -hv a2 -f=farg", ' ' as u8);

        let rest = p.parse(argv);
        assert len(rest) == 2u;
        assert rest[0] == "a1";
        assert rest[1] == "a2";

        assert p.is_set("-help");
        assert p.is_set("-version");
        assert p.is_set("-I");
        assert p.is_set("-file");
        // any flag should report the same answer
        assert p.is_set("-f") == p.is_set("-file") == p.is_set("--file");

        assert p.get("-file") == "farg";
        let iargs = p.get_many("-I");
        assert len(iargs) == 2u;
        assert iargs[0] == "/inc";
        assert iargs[1] == "/inc2";

        p.reset();
        assert ! p.is_set("-file");
        assert ! p.is_set("-I");
        assert ! p.is_set("-help");
        assert ! p.is_set("-version");
    }


    #[test]
    fn test_option() {

        // boolean option
        let b_option = new_bool_option("-h -help");
        assert ! b_option.is_set();
        b_option.set();
        assert b_option.is_set();
        b_option.reset();
        assert ! b_option.is_set();

        // string options
        let s_option = new_str_option_fancy("-f --file");
        assert ! s_option.is_set();
        s_option.add("one");
        assert s_option.is_set();
        
        let s_state = s_option.state();
        assert len( (*s_state).str_state ) == 1u;

        s_option.add("two");
        assert s_option.is_set();

        let s_state = s_option.state();
        assert len( (*s_state).str_state ) == 2u;
        assert (*s_state).str_state[0] == "one";
        assert (*s_state).str_state[1] == "two";
    }

    #[test]
    fn test_juxta_split() {
        let sp = juxta_split("-abc");
        assert len(sp) == 3u;
        assert sp[0] == "-a";
        assert sp[1] == "-b";
        assert sp[2] == "-c";
    }

    #[test]
    fn test_fancy_options() {

        let fncy:str   = fancy_options("-a --all");
        let sp:[str]   = split(fncy, ' ' as u8);
        let ok:[str]   = ["-a","-a=","-all","-all=","--all","--all="];
        let sort_ok    = merge_sort(std::str::lteq, ok);
        let sort_sp    = merge_sort(std::str::lteq, sp);
        assert len(sort_sp) == len(sort_ok);

        let i = 0u;
        let max = len(sort_sp);

        while i < max {
            assert sort_ok[i] == sort_sp[i];
            i += 1u;
        }
    }
}

// howto in here
fn main(args: [str]){

    let p = ropt::new_parser();

    p.add_bool("-h -help --help");
    p.add_bool("-v -version --version");
    p.add_bool("-V -verbose --verbose");
    p.add_str("-I -I=");
    // same as: p.add_str("-f -f= -file -file= --file --file=")
    p.add_str_fancy("-f --file"); 

    let arg = p.parse(args);

    println("---------- 1.0 ----------");

    if p.is_set("-help") {
        println("i'm helping you right now");
    }

    if p.is_set("-version") {
        println("version 1.0");
    }

    if p.is_set("-verbose") {
        println("hi, my name is dr.phil; in tonights episode we...");
    }

    if p.is_set("-file") {
        println(#fmt("-file: %s", p.get("-file")));
    }

    if p.is_set("-I") {
        let is = p.get_many("-I");
        for i in is {
            println(#fmt("-I: %s", i));
        }
    }

    for a in arg {
        println(#fmt("arg: %s", a));
    }

    // another way to see which options were set. the shortest option
    // for each set flag is returned; we switch (or alternate) through
    // the list of flags of all the options that was given...
    println("---------- 2.0 ----------");

    for set_option in p.short_set_options() {
        alt set_option {
            "-h" {
                println("v2 help");
            }
            "-v" {
                println("v2 version");
            }
            "-V" {
                println("v2 verbose");
            }
            "-f" {
                println(#fmt("v2 file: %s", p.get("-file")));
            }
            "-I" {
                println(#fmt("v2 -I is set"));
                let is = p.get_many("-I");
                for i in is {
                    println(#fmt("-I: %s", i));
                }
            }
            _ { fail("unknown flag was set?"); }
        }
    }

    p.reset();

    assert ! p.is_set("-file");
    assert ! p.is_set("-I");
    assert ! p.is_set("-version");
    assert ! p.is_set("-help");

}
