class CodeGen(var program : Program, var options : CoolOptions, var output : IO)
extends CoolVisitor()
{
  def run() : Unit = {
    program.accept(this); ()
  };

  var emitter : Emitter = new Emitter(output,options.get_enable_gc());

  var code_info_table : CodeInfoTable = null;

  override def visit_program(the_node:Cprogram,classes:Classes) : Any = {

    code_info_table = new CodeInfoTable(options,classes);    
    code_info_table.build_inher_graph(the_node.get_any_class());
    code_info_table.assign_feature_offsets();
    // PA6: set offsets
    
    // Used later for protObj generation
    code_info_table.set_int_class(the_node.get_int_class());
    code_info_table.set_boolean_class(the_node.get_boolean_class());
    code_info_table.set_unit_class(the_node.get_unit_class());
    code_info_table.set_string_class(the_node.get_string_class());
    
    var lit_build : LiteralTableBuilder = new LiteralTableBuilder(classes);
    var int_lit_table : Hashtable = lit_build.get_int_lit_table();
    var str_lit_table : Hashtable = lit_build.get_str_lit_table();

    // PA6: control all the setting of tags, generation of
    // literals, generation of prototype objects, generation of 
    // class name table.

    // EMIT 

    emit_boilerplate();

    // Unit literal
    var cl_tag : Int = code_info_table.get_class_tag(the_node.get_unit_class());
    emitter.object_header(emitter.s_UNITLIT(),cl_tag,3,"Unit");
  
    // Boolean literals
    cl_tag = code_info_table.get_class_tag(the_node.get_boolean_class());
    emitter.object_header(emitter.s_BOOLEANLIT(false),cl_tag,4,"Boolean");
    emitter.word("0");
    emitter.object_header(emitter.s_BOOLEANLIT(true),cl_tag,4,"Boolean");
    emitter.word("1");

    // Int literals
    cl_tag = code_info_table.get_class_tag(the_node.get_int_class());
    var int_lit_enum : Enumeration = int_lit_table.elements();
    while (int_lit_enum.hasNext()) {
      var value : Symbol = null;
      var id : Int = -1;
      int_lit_enum.next() match {
        case null => // Should never occur!
        case p:Pair => 
          value = p.first() match { case x:Symbol => x };
          id = p.second() match { case y:Int => y }
      };
      emitter.object_header(emitter.s_INTLIT(id),cl_tag,emitter.i_WORDSIZE(),"Int");
      emitter.word(symbol_name(value))
    };

    // String literals
    cl_tag = code_info_table.get_class_tag(the_node.get_string_class());
    var str_lit_enum : Enumeration = str_lit_table.elements();
    while (str_lit_enum.hasNext()) {
      var value : Symbol = null;
      var id : Int = -1;
      str_lit_enum.next() match {
        case null => // Should never occur!
        case p:Pair =>
          value = p.first() match { case x:Symbol => x };
          id = p.second() match { case y:Int => y }
      };
      var str_length : Int = symbol_name(value).length();
      var obj_size : Int = 5 + str_length / 4;
      emitter.object_header(emitter.s_STRINGLIT(id),cl_tag,obj_size,"String");
      var length_id : Int = int_lit_table.get(symbol(str_length.toString())) match {
        case null => -1
        case x:Int => x
        case y:Any => -1
      };
      emitter.word(emitter.s_INTLIT(length_id));
      emitter.string_attr(value);
      ()
    };

    // Class Name Table
    code_info_table.emit_class_name_table(emitter,str_lit_table);

    // Dispatch tables and prototype objs
    code_info_table.emit_disp_tables(emitter);

    emitter.text();

    // needed to make spim happy
    // For some reason, this can't be put in cool-runtime.s
    emitter.label_def("main");
    emitter.opc("jr").opn(emitter.s_RA()).endl(program);
    emitter.endl(program);

    emitter.data();
    emitter.global_def(emitter.s_HEAPSTART())
  };

  def emit_boilerplate() : Unit = {
    emitter.global(emitter.s_CLASSNAMETAB());
    emitter.global("Main".concat(emitter.s_PROTOBJ()));
    emitter.global("Int".concat(emitter.s_PROTOBJ()));
    emitter.global("Boolean".concat(emitter.s_PROTOBJ()));
    emitter.global("String".concat(emitter.s_PROTOBJ()));
    emitter.global("Symbol".concat(emitter.s_PROTOBJ()));
    emitter.global(emitter.s_UNITLIT());
    emitter.global(emitter.s_FALSE());
    emitter.global(emitter.s_TRUE());
    ()
  };
}


/** A class to handle formatting the assembly file.
 */
class Emitter(var output : IO, var enable_gc : Boolean) extends IO()
{
  // { new IO().out("enable_gc = ").out_any(enable_gc).out("\n") };
  override def out(s : String) : IO = output.out(s);

  // conventions
  def s_CLASSNAMETAB() : String = "class_nameTab";
  def s_CLASSOBJTAB() : String = "class_objTab";
  def s_HEAPSTART() : String = "heap_start";
  def s_DISPTAB() : String = "_dispTab";
  def s_PROTOBJ() : String = "_protObj";
  def s_METHODSEP() : String = ".";
  def s_ANYCLONE() : String = "Any.clone";
  def s_ERRORABORT() : String = "_error_abort";

  def s_METHOD(cn : Symbol, mn : Symbol) : String =
    symbol_name(cn).concat(s_METHODSEP()).concat(symbol_name(mn));
  
  def s_INTLIT(offset : Int) : String = 
    "int_lit".concat(offset.toString());
  def s_STRINGLIT(offset : Int) : String = 
    "string_lit".concat(offset.toString());
  def s_BOOLEANLIT(value : Boolean) : String =
    if (value) "boolean_lit1" else "boolean_lit0";
  def s_UNITLIT() : String = "unit_lit";

  def s_FALSE() : String = s_BOOLEANLIT(false);
  def s_TRUE() : String = s_BOOLEANLIT(true);

  def s_DISPATCHABORT() : String = "_dispatch_abort";
  def s_DIVABORT() : String = "_divide_abort";
  def s_CASEABORT() : String = "_case_abort";

  def i_WORDSIZE() : Int = 4;
  def i_TAGOFFSET() : Int = 0;
  def i_SIZEOFFSET() : Int = 1;
  def i_DISPTABOFFSET() : Int = 2;
  def i_ATTROFFSET() : Int = 3;
  def i_METHODOFFSET() : Int = 0;

  def s_ZERO() : String = "$zero";
  def s_ACC() : String = "$a0";
  def s_AUX() : String = "$a1"; // for some primitive functions
  def s_SELF() : String = "$s0";
  def s_T1() : String = "$t1";
  def s_T2() : String = "$t2";
  def s_T3() : String = "$t3";
  def s_SP() : String = "$sp";
  def s_FP() : String = "$fp";
  def s_RA() : String = "$ra";

  { out("\t.data\n") };
  var mode : Int = 1; // 0 = text, 1 = data, 2 = in ascii

  def gen_offset(s : String, o : Int) : String =
    (o*i_WORDSIZE()).toString().concat("(").concat(s).concat(")");

  def opc(s : String) : Emitter = {out("\t").out(s).out("\t");this};
  def opn(s : String) : Emitter = {out(" ").out(s); this};
  def offset(s : String, o : Int) : Emitter = opn(gen_offset(s,o));

  var last_line : Int = 0;
  def endl(n : CoolNode) : Unit = {
    var l : Int = n.get_line_number();
    if (l == last_line) ()
    else if (l == 0) ()
    else { out("\t# line ").out_any(l); last_line = l };
    out("\n"); ()
  };

  def set_mode(n : Int) : Unit = {
    while (!(mode == n)) {
      if (mode == 2) {
	out("\"\n");
	mode = 1
      } else if (mode == 0) {
	out("\t.data\n");
	mode = 1
      } else if (n == 0) {
	out("\t.text\n");
	mode = 0
      } else {
	opc(".ascii").out("\"");
	mode = 2
      }
    }
  };

  def text() : Unit = set_mode(0);
  def data() : Unit = set_mode(1);
  def ascii() : Unit = set_mode(2);

  def char(ch : String) : Unit = {
    ascii();
    out(ch);
    ()
  };

  def byte(i : Int) : Unit = {
    data();
    opc(".byte").out_any(i).out("\n");
    ()
  };

  def align() : Unit = { 
    data(); 
    opc(".align").out_any(2).out("\n"); 
    ()
  };

  def global(name : String) : Any = opc(".globl").out(name).out("\n");
  def label_def(name : String) : Any = out(name).out(":\n");
  def global_def(name : String) : Any = { global(name); label_def(name) };

  def word(x : Any) : Any = out("\t.word\t").out(x.toString()).out("\n");

  {
    data();
    global_def("_MemMgr_INITIALIZER");
    word(if (enable_gc) "_GenGC_Init" else "_NoGC_Init");
    global_def("_MemMgr_COLLECTOR");
    word(if (enable_gc) "_GenGC_Collect" else "_NoGC_Collect");
    global_def("_MemMgr_TEST");
    word(0)
  };

  /** Generate code to record file and line numebr and then jump
   * to the appropriate error reporting routine in the runtime.
   */
  // An example of a helper method.  
  // The solution uses this whenever it needs to handle an "abort"
  def error_abort(target: String, fi : Int, li : Int) : Unit = {
    opc("la").opn(s_AUX()).opn(s_STRINGLIT(fi)).out("\n");
    opc("li").opn(s_T1()).opn(li.toString()).out("\n");
    opc("j").opn(target).out("\n");
    ()
  };

  // CS 654 students don't need to call this method
  // Only use if garbage collection is enabled.
  def gc_assign(offset : Int) : Unit = {
    opc("la").opn(s_AUX()).offset(s_SELF(),offset+i_ATTROFFSET()).out("\n");
    opc("jal").opn("_GenGC_Assign").out("\n");
    ()
  };

  // Method for generating an object header.
  def object_header(label : String, tag : Int, size : Int, tab : String) : Unit = {
    word("-1");
    label_def(label);
    word(tag.toString());
    word(size.toString());
    word(tab.concat(s_DISPTAB()));
    ()
  };

  // Method for generating attr's of string literal.
  def string_attr(value : Symbol) : Unit = {
    var s : String = symbol_name(value);
    var sb : StringBuilder = new StringBuilder();
    var i : Int = 0;
    var j : Int = 0;
    var k : Int = s.length();
    while (i < k) {
      if (is_control_char(s.charAt(i))) {
        if (!(i == j)) char(s.substring(j,i)) else ();
        byte(s.charAt(i));
        j = i + 1
      } else ();
      i = i + 1
    };
    if (!(i == j)) char(s.substring(j,i)) else ();
    byte(0);
    align()
  };

  def is_control_char(c : Int) : Boolean = {
    if (c < 32) true 
    else if (c == 92) true
    else c == 127
  };
}

class CodeInfoTable(var options : CoolOptions, var classes : Classes) 
extends CoolVisitor()
{
  var table : ArrayAny = new ArrayAny(100);
  var tags_assigned : Boolean = false;
  var graph_built : Boolean = false;
  var inher_root : ClassInfo = null;

  // Helper fields and methods
  var int_class : Class = null;
  var boolean_class : Class = null;
  var unit_class : Class = null;
  var string_class : Class = null;
  def set_int_class(nd : Class) : Unit = int_class = nd;
  def set_boolean_class(nd : Class) : Unit = boolean_class = nd;
  def set_unit_class(nd : Class) : Unit = unit_class = nd;
  def set_string_class(nd : Class) : Unit = string_class = nd;
  
  // Helper method in setting the init
  // value of attributes of protObjs.
  def init_class_value(cldc : Class) : String = {
    if (cldc == int_class) "int_lit0"
    else if (cldc == boolean_class) "boolean_lit0"
    else if (cldc == unit_class) "unit_lit"
    else "0"
  };

  { 
    classes.accept(this);
    tags_assigned = true
  };

  override def visit_Classes_one(cls : Class) : Any = cls.accept(this);

  override def visit_class_decl(cl:Cclass_decl,n:Symbol,p:Symbol,f:Features,fname:Symbol) : Any = {
    if (!tags_assigned) {     
      // PHASE 1
      set_info(cl, new ClassInfo(cl))
    } else if (!graph_built) {
      // PHASE 2
      var info : ClassInfo = get_class_info(cl);
      if (!(info == inher_root)) {
        get_class_info(cl.get_superclass()).add_child(info)
      } else ()
    } else ()
  };

  override def visit_Features_one(f : Feature) : Any = f.accept(this);
  
  def get_raw_info(n : CoolNode) : Any =
    if (table.length() <= n.get_id()) null else table.get(n.get_id());

  def set_info(n : CoolNode, v : Any) : Unit = {
    while (table.length() <= n.get_id()) table = table.resize(table.length()*2);
    table.set(n.get_id(),v); ()
  };

  def get_offset_info(n : CoolNode) : Int = {
    get_raw_info(n) match { 
      case null => -1
      case i:Int => i
      case x:Any => -1
    }
  };

  def build_inher_graph(root : Class) : Unit = {
    inher_root = get_class_info(root);
    classes.accept(this);
    inher_root.assign_tags(0);
    graph_built = true
  };

  def assign_feature_offsets() : Unit = {
    inher_root.assign_offsets(this,0,0)
  };

  def get_class_tag(node : Class) : Int = {
    get_class_info(node).get_class_tag()
  };

  def get_class_info(node : Class) : ClassInfo = {
    get_raw_info(node) match {
      case null => null
      case x:ClassInfo => x
      case y:Any => null
    }
  };

  // Emit class name table.
  // Precondition - class tags assigned, inher tree built
  def emit_class_name_table(e : Emitter, str_tab : Hashtable) : Unit = {
    e.label_def(e.s_CLASSNAMETAB());
    inher_root.emit_class_name_table(e,str_tab)
  };
  
  // Emit disp tables and protObjs for all classes
  // starting from the root of the inheritance tree.
  // Complex precondition :
  //    - class tags assigned
  //    - inher tree built
  //    - feature offsets assigned
  def emit_disp_tables(e : Emitter) : Unit = {
    inher_root.emit_code_info(e,this)
  };
}

// Helper class involved in almost every aspect of PA6.
// Used to construct inheritance tree, and providing the ability
// to traverse the inher tree in a veriety of different ways.
class ClassInfo(var c : Cclass_decl) extends IO() {
  var tag : Int = -1;
  var first_child_info : ClassInfo = null;
  var next_child_info : ClassInfo = null;
  var is_root : Boolean = false;
  var num_meth : Int = 0;
  var num_attr : Int = 0;
  var over_head : MNode = null;
  var meth_entries : ArrayAny = null;
  var myName : String = symbol_name(c.get_name());

  // Getters
  def get_class() : Class = c match { case x:Class => x };
  def get_class_tag() : Int = tag;
  def get_first_child_info() : ClassInfo = first_child_info;
  def get_next_child_info() : ClassInfo = next_child_info;
  def get_meth_entries() : ArrayAny = meth_entries;

  def add_child(ci : ClassInfo) : Unit = {
    ci.set_sibling(first_child_info);
    first_child_info = ci
  };

  def set_sibling(ci : ClassInfo) : Unit = next_child_info = ci;

  // Assign class tag to this class and recurse to children (preorder).
  def assign_tags(i : Int) : Int = {
    tag = i;
    is_root = (tag == 0);
    var next : Int = i + 1;
    var child : ClassInfo = first_child_info;
    while (!is_null(child)) {
      next = child.assign_tags(next);
      child = child.get_next_child_info()
    };
    next
  };

  // Recursive method which assigns offsets to the features belonging to this class.
  // Precondition - class tags assigned, inheritance graph constructed
  def assign_offsets(cit : CodeInfoTable, attrVal : Int, methVal : Int) : Unit = {
    var curAttrVal : Int = attrVal;
    var curMethVal : Int = methVal;
    var tail : MNode = null;
    var feat_enum : FeaturesEnumeration = c.get_features().elements();

    // Assign offsets to all features of this class as well as other
    // work to assist in future traversals (dispTabs, protObjs, etc).
    while (feat_enum.hasNext()) {
      feat_enum.next() match {
        case x:Cattr =>
          cit.set_info(x, curAttrVal);
          curAttrVal = curAttrVal + 1
        case y:Cmethod => 
          if (is_null(y.get_overrides())) {
            // Override case
            cit.set_info(y, curMethVal);
            curMethVal = curMethVal + 1
          } else {
            // Standard case
            cit.set_info(y,cit.get_offset_info(y.get_overrides()))
          };
          
          // Add nodes to primitive un-encapsulated linked list.
          // It is used in emitting dispatch tables (See emit_class_info(...))
          if (is_null(tail)) {
            // Add on empty list
            over_head = new MNode(y,null);
            tail = over_head
          } else {
            // Add on non-empty list
            tail.set_next(new MNode(y,null));
            tail = tail.get_next()
          }
      }
    };

    // Set late attributes
    num_meth = curMethVal;
    num_attr = curAttrVal;
    
    // Continue down the tree
    var child : ClassInfo = first_child_info;
    while (!is_null(child)) {
      child.assign_offsets(cit,curAttrVal,curMethVal);
      child = child.get_next_child_info()
    }
  };

  // Emit class name table by traversing the inheritance tree (preorder).
  def emit_class_name_table(e : Emitter, t : Hashtable) : Unit = {
    t.get(c.get_name()) match { case i:Int => e.word(e.s_STRINGLIT(i)) };
    var child : ClassInfo = first_child_info;
    while (!is_null(child)) {
      child.emit_class_name_table(e,t);
      child = child.get_next_child_info()
    }
  };

  // This method controls the order of which emit_class_info is called
  // on the nodes of the inheritance graph. Specifically, it uses a
  // preorder traversal in ultimately emitting dispatch and protObj code.
  // See the preconditions below (emit_class_info) as they apply here too!
  def emit_code_info(e : Emitter, ct : CodeInfoTable) : Unit = {
    emit_class_info(e,ct);
    var child : ClassInfo = first_child_info;
    while (!is_null(child)) {
      child.emit_code_info(e,ct);
      child = child.get_next_child_info()
    }
  };
  
  // Emit dispatch table and protObj for this class.
  // This method has complex preconditions :
  //    - inheritance graph constructed
  //    - relies on the parent having already called emit_class_info
  //    - method offsets assigned
  // This method also calls emit_attr_info which recurses up the 
  // inheritance graph building an attribute table for this class
  // which includes all superclass attr's.
  def emit_class_info(e : Emitter, ct : CodeInfoTable) : Unit = {
    if (is_root) meth_entries = new ArrayAny(num_meth)
    else {
      var p_info : ClassInfo = ct.get_class_info(c.get_superclass());
      meth_entries = p_info.get_meth_entries().resize(num_meth)
    };

    var current : MNode = over_head;
    while (!is_null(current)) {
      var offset : Int = ct.get_offset_info(current.get_method());
      meth_entries.set(offset,e.s_METHOD(c.get_name(),current.get_name()));
      current = current.get_next()
    };

    e.label_def(myName.concat(e.s_DISPTAB()));

    // Emit dispatch table
    var index : Int = 0;
    while (index < meth_entries.length()) {
      meth_entries.get(index) match {
        case null => 
        case x:String => e.word(x)
        case y:Any =>
      };
      index = index + 1
    };

    // Special global protObj if ArrayAny, Symbol, Boolean, String, or Int
    if (myName == "ArrayAny") e.global(myName.concat(e.s_PROTOBJ()))
    else if (myName == "Symbol") e.global(myName.concat(e.s_PROTOBJ()))
    else if (myName == "Boolean") e.global(myName.concat(e.s_PROTOBJ()))
    else if (myName == "String") e.global(myName.concat(e.s_PROTOBJ()))
    else if (myName == "Int") e.global(myName.concat(e.s_PROTOBJ()))
    else ();


    // Emit prototype object
    e.object_header(myName.concat(e.s_PROTOBJ()),tag,num_attr+3,myName);
    emit_attr_info(e,ct);
    ()
  };

  // Emit attributes for protObj
  def emit_attr_info(e : Emitter, ct : CodeInfoTable) : Unit = {
    if (!is_root) ct.get_class_info(c.get_superclass()).emit_attr_info(e,ct) else ();
    var enum : FeaturesEnumeration = c.get_features().elements();
    while (enum.hasNext()) {
      enum.next() match {
        case x:Cmethod =>
        case y:Cattr => 
          var of_class : Class = y.get_feature_of_class();
          e.word(ct.init_class_value(of_class))
      }
    };
    ()
  };
}

// Helper class used to wrap around a Cmethod AST Node.
// Used primarily to assist in emitting dispatch table info.
class MNode(var method : Cmethod, var next : MNode) {
  def get_method() : Cmethod = method;
  def get_next() : MNode = next;
  def set_method(m : Cmethod) : Unit = method = m;
  def set_next(n : MNode) : Unit = next = n;
  def get_name() : Symbol = method.get_name();
}

// Visitor which builds literal tables.
class LiteralTableBuilder(var classes : Classes) extends CoolTreeVisitor() {
  var ilt : Hashtable = new Hashtable();  // int lit table
  var slt : Hashtable = new Hashtable();  // string lit table

  def get_int_lit_table() : Hashtable = ilt;

  def get_str_lit_table() : Hashtable = slt;
  
  // Assign id to int_lit iff if it has not been scanned already.
  def scan_int_lit(lit : Symbol) : Unit = {
    if (is_null(ilt.get(lit))) { 
      ilt.put(lit,ilt.size());
      ()
    } else ()
  };
  
  // Assign id to string_lit iff it has not been scanned already.
  // Furthermore, it scans an int literal of value equal to the 
  // length of this string_lit, implicitly handling the length 
  // attribute of string literals.
  def scan_str_lit(lit : Symbol) : Unit = {
    if (is_null(slt.get(lit))) {
      var id : Int = slt.put(lit,slt.size()) match { 
        case null => -1
        case x:Int => x 
        case y:Any => -1
      };
      var imp_size : Int = symbol_name(lit).length();
      scan_int_lit(symbol(imp_size.toString()));
      ()
    } else ()
  };


  // Visit all class_decls, int_lits, and string_lits and scan them into the table.
  //##############################################################################

  { classes.accept(this) };

  override def visit_class_decl(c:Cclass_decl,n:Symbol,p:Symbol,f:Features,fn:Symbol) : Any = {
    scan_str_lit(n);
    scan_str_lit(p);
    scan_str_lit(fn);
    f.accept(this)
  };

  override def visit_int_lit(n:Cint_lit,t:Symbol) : Any = {
    scan_int_lit(t)
  };

  override def visit_string_lit(n:Cstring_lit,t:Symbol) : Any = {
    scan_str_lit(t)
  };

  //##############################################################################
}

class PrintCodeInfo(var code_info_table : CodeInfoTable) extends CoolTreeVisitor() {
  def run(p : Program) : Any = {
    p.accept(this)
  };

  override def visit_class_decl(the_node:Cclass_decl,name:Symbol,parent:Symbol,features:Features,filename:Symbol) : Any = {
    out("class tag for ").out(symbol_name(name)).out(" = ");
    out_any(code_info_table.get_class_tag(the_node));
    out("\n");
    super.visit_class_decl(the_node,name,parent,features,filename)
  };

  override def visit_method(m:Cmethod,o:Boolean,name:Symbol,fs:Formals,
                            rtype:Symbol,expr:Expression) : Any = {
    out("  method tag for ").out(symbol_name(name)).out(" = ");
    out_any(code_info_table.get_offset_info(m));
    out("\n");
    super.visit_method(m,o,name,fs,rtype,expr)
  };

  override def visit_attr(a:Cattr,name:Symbol,of_type:Symbol) : Any = {
    out("  attr tag for ").out(symbol_name(name)).out(" = ");
    out_any(code_info_table.get_offset_info(a));
    out("\n");
    super.visit_attr(a,name,of_type)
  };
}

