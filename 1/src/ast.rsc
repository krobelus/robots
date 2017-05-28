module ast

import Prelude;

data Script = _script(str name, list[TopLevelCommand] commands); 

data TopLevelCommand =
    _routine(str name, list[Command] body)
  | _c(Command command)
  ;

data Command =
    _gc(GridCommand gridCommand)
  | _rc(RobotCommand robotCommand)
  | _cfc(ControlFlowCommand controlFlowCommand)
  | _cc(str comment)
  | _call(str name)
  ;

data GridCommand =
    _buildWall(int row, int col)
  | _destroyWall(int row, int col)
  | _dropMark(int row, int col)
  | _pickMark(int row, int col)
  ;

data RobotCommand =
    _step()
  | _turnLeft()
  | _pick()
  | _trace(str message)
  ;

data ControlFlowCommand =
    _ifelse(OrExpression cond, list[Command] then, list[Command] _else)
  | _if(OrExpression cond, list[Command] then)
  | _while(OrExpression cond, list[Command] body)
  | _repeat(int times, list[Command] body)
  ;

data OrExpression =
	_or(AndExpression left, AndExpression right)
  | _noor(AndExpression child)
  ;

data AndExpression =
	_and(LogicalExpression left, LogicalExpression right)
  | _noand(LogicalExpression child)
  ;

data LogicalExpression =
    _full()
  | _mark()
  | _wallAhead()
  | _heading(Direction direction)
  | _not(LogicalExpression expression)
  ;

data Direction = _south() | _north() | _west() | _east();

public str show(_south()) { return "south"; }
public str show(_north()) { return "north"; }
public str show(_west()) { return "west"; }
public str show(_east()) { return "east"; }

public str show(_or(left, right))  { return "<show(left)> or <show(right)>"; }
public str show(_and(left, right)) { return "<show(left)> and <show(right)>"; }
public str show(_noor(child))  { return "<show(child)>"; }
public str show(_noand(child))  { return "<show(child)>"; }
public str show(LogicalExpression e) {
  if (_full() := e) {
    return "full";
  } else if (_mark() := e) {
    return "mark";
  } else if (_wallAhead() := e) {
    return "wallAhead";
  } else if (_heading(direction) := e) {
    return "heading <show(direction)>";
  } else if (_not(expr) := e) {
    return "not <show(expr)>";
  } else {
  	return "*** error: don\'t know how to print this expression";
  }
}

public str show(Command c, int i) {
  str s = intercalate("", ["    " | _ <- [0 .. i] ]);
  if (_gc(_buildWall(row:_, col:_)) := c) {
    s += "buildWall at row: <row> col: <col>";
  } else if (_gc(_destroyWall(row:_, col:_)) := c) {
    s += "destroyWall at row: <row> col: <col>";
  } else if (_gc(_dropMark(row:_, col:_)) := c) {
    s += "dropMark at row: <row> col: <col>";
  } else if (_gc(_pickMark(row:_, col:_)) := c) {
    s += "pickMark at row: <row> col: <col>";
  } else if (_rc(_step()) := c) {
    s += "step";
  } else if (_rc(_turnLeft()) := c) {
    s += "turnLeft";
  } else if (_rc(_pick()) := c) {
    s += "pick";
  } else if (_rc(_trace(message:_)) := c) {
    s += "trace <message>";
  } else if (_cfc(_ifelse(cond:_, then:_, e:_)) := c) {
    s += "if <show(cond)> do\n<show(then, i + 1)>\n<s>end else do\n<show(e, i + 1)>\n<s>end";
  } else if (_cfc(_if(cond:_, then:_)) := c) {
    s += "if <show(cond)> do\n<show(then, i + 1)>\n<s>end";
  } else if (_cfc(_while(cond:_, body:_)) := c) {
    s += "while <show(cond)> do\n<show(body, i + 1)>\n<s>end";
  } else if (_cfc(_repeat(times:_, body:_)) := c) {
    s += "repeat <times> times\n<show(body, i + 1)>\n<s>end";
  } else if (_cc(comment:_) := c) {
    s += "<comment>";
  } else if (_call(name:_) := c) {
    s += name;
  } else {
  	s += "*** error: don\'t know how to print this command";
  }
  return s;
}

public str show(TopLevelCommand c) {
  if (_routine(name:_, body:_) := c) {
    return "    routine <name> means\n<show(body, 2)>\n    end";
  } else if (_c(command:_) := c) {
    return show(command, 1);
  }
}

public str show(list[Command] commands, int indentation) {
  return intercalate("\n", [show(c, indentation) | c <- commands]);
}

public str show(_script(name, commands)) {
  return "Script <name> runs as\n"
       + "<intercalate("\n", [show(c) | c <- commands])>\n"
       + "end\n"
       ;
}

public void printScript(_script(name, commands)) {
  println(show(_script(name, commands)));
}
