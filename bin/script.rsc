module script

import ParseTree;
import Prelude;

import ast;

lexical Whitespace = [\ \t\n\r];
lexical Name = [a-zA-Z] !<< [a-zA-Z]+ !>> [a-zA-Z] \ Keywords;
lexical Integer = [0-9]+;
lexical Comment = [#] ![\n]* [\n];
lexical String = [\"] ![\"]* [\"];

layout Layout = Whitespace* !>> [\ \t\n\r];

keyword Keywords = "Script" | "runs" | "as" | "step" | "turnLeft" | "drop" | "pick" | "trace" | "if" | "do" | "end" | "else" | "while" | "repeat" | "times" | "full" | "mark" | "wall" | "ahead" | "heading" | "south" | "north" | "west" | "east" | "buildWall" | "destroyWall" | "dropMark" | "pickMark"; 

start syntax Script = _script: "Script" Name "runs" "as" TopLevelCommand* "end";

syntax TopLevelCommand =
    _routine: "routine" Name "means" Command* "end"
  | _c: Command 
  ;
	
syntax Command =
    _gc: GridCommand
  | _rc: RobotCommand
  | _cfc: ControlFlowCommand
  | _cc: Comment
  | _call: Name
  ;


syntax GridCommand =
    _buildWall: "buildWall" "at" "row" ":" Integer "col" ":" Integer
  | _destroyWall: "destroyWall" "at" "row" ":" Integer "col" ":" Integer
  | _dropMark: "dropMark" "at" "row" ":" Integer "col" ":" Integer
  | _pickMark: "pickMark" "at" "row" ":" Integer "col" ":" Integer
  ;

syntax RobotCommand =
    _step: "step"
  | _turnLeft: "turnLeft"
  | _drop: "drop"
  | _pick: "pick"
  | _trace: "trace" String
  ;

syntax ControlFlowCommand =
    _ifelse: "if" OrExpression "do" Command* "end" "else" "do" Command* "end"
  | _if: "if" OrExpression "do" Command* "end"
  | _while: "while" OrExpression "do" Command* "end"
  | _repeat: "repeat" Integer "times" Command* "end"
  ;
  
syntax OrExpression =
    _or: AndExpression "or" AndExpression
  | _noor: AndExpression
  ;

syntax AndExpression =
	_and: LogicalExpression "and" LogicalExpression
  | _noand: LogicalExpression
  ;

syntax LogicalExpression =
    _full: "full"
  | _mark: "mark"
  | _wallAhead: "wall" "ahead"
  | _heading: "heading" Direction
  | _not: "not" LogicalExpression
  ;

syntax Direction =
	_south: "south" | _north: "north" | _west: "west" | _east: "east"; 
	
public str sample = "
Script sample runs as
		while not mark and wall ahead do
				step
		end
end
";

public str example1 = "
Script sample runs as
	# turn right
	repeat 3 times
			turnLeft
	end
	
	step
	
	if mark do
			pick
			trace \"Found and picked the mark!\"
	end
end
";

public str example2 = "
Script sample runs as
		while not heading south do
			turnLeft
		end

		while not mark and not wall ahead do
				step
		end
end
";

public str example3 = "
Script sample runs as
	routine turnRight means
		repeat 3 times
			turnLeft
		end
	end

	# now we can call turnRight
	turnRight
end
";

public str example4 = "
Script sample runs as
	buildWall at row: 1 col: 1
	buildWall at row: 2 col: 1
	buildWall at row: 3 col: 1
	buildWall at row: 1 col: 9
	routine foo means
		step
	end

    if not mark or not full do
    	foo
    end else do
    	while wall ahead do
			turnLeft
		end
    end

	foo
end
";

public tuple[bool, ast::Script] parseScript(str input) {
	ast::Script script = implode(#ast::Script, parse(#start[Script], input));
	_script(_, commands) = script;
	set[str] routines = {};
	for (c <- commands) {
		if(_routine(rname:_, rcommands:_) := c) {
			routines += rname;
		} else if (_c(call(rname:_)) := c) {
			if (!(rname in routines)) {
				return <false, script>;
			}
		}
	}
	return <true, script>;
}

public void process(str input) {
	<success, script> = parseScript(input);
	if (!success) {
		println("invalid input!!");
	} else {
		printScript(script);
	}
}

public void main(list[str] args) {
	process(example1);
	process(example2);
	process(example3);
	process(example4);
}
