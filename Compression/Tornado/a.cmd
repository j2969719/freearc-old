call bench0.cmd d:\temp\dll
call bench1.cmd d:\temp\dll
call bench.cmd d:\temp\dll

call bench0.cmd d:\temp\all
call bench1.cmd d:\temp\all
call bench.cmd d:\temp\all

call bench0.cmd d:\temp\dict
call bench1.cmd d:\temp\dict
call bench.cmd d:\temp\dict

@read d:\temp\dll >nul
t thor e1 d:\temp\dll nul|tail -5
t thor e2 d:\temp\dll nul|tail -5
t thor e3 d:\temp\dll nul|tail -5
t thor e4 d:\temp\dll nul|tail -5
t thor e5 d:\temp\dll nul|tail -5
@read d:\temp\all >nul
t thor e1 d:\temp\all nul|tail -5
t thor e2 d:\temp\all nul|tail -5
t thor e3 d:\temp\all nul|tail -5
t thor e4 d:\temp\all nul|tail -5
t thor e5 d:\temp\all nul|tail -5
@read d:\temp\dict >nul
t thor e1 d:\temp\dict nul|tail -5
t thor e2 d:\temp\dict nul|tail -5
t thor e3 d:\temp\dict nul|tail -5
t thor e4 d:\temp\dict nul|tail -5
t thor e5 d:\temp\dict nul|tail -5
