import getopt, string, posixpath, sys, os, os.path, re

# Some global globals...
verbose     = 0
thisProg    = posixpath.basename(sys.argv[0])
if not thisProg: # happy only when running in xemacs ;/
    thisProg = 'am_edit.py'
cppsuffixes = ['cpp', 'cc', 'cxx', 'C', 'c++']
hExt        = ['h', 'H', 'hh', 'hxx', 'h++']
progId      = "KDE tags expanded automatically by " + thisProg
use_final   = 1
dryrun      = 0
pathoption  = 0
topdir      = os.path.abspath(os.curdir) + "/"
foreigndirs = []

class Makefile:
    def __init__(self, file):
        # some useful globals for the subroutines called here
        self.headerdirs = ['.']
        self.haveAutomocTag   = 0

        self.programs = []

        # lists the objects compiled into $program
        self.realobjs = {}
        # lists the sources used for $program
        self.sources = {}
        # lists the objects compiled when final
        self.finalObjs = {}
        # the binary name of program variable
        self.realname = {}
        # lists the idl files used for $program
        self.idlfiles = {}
        # lists all idl generated files for cleantarget
        self.idl_output = ""

        self.depedmocs = {}

        self.dep_files      = ""
        self.dep_finals     = ""
        # the targets to add
        self.target_adds    = {}
        self.kdelang        = ""
        self.makefile       = file
        self.makefileDir    = os.path.dirname(self.makefile)
        self.options        = {}


    NoMakefileAmFound = "found Makefile.in without Makefile.am"

    def findLine(self, line):
        import types
        if type(line) is types.StringType:
            regexp = re.compile(line)
        else:
            regexp = line
        for line in self.lines:
            match = regexp.match(line)
            if match:
                return match
    
    def substituteLine(self, old, new):
        import types
        if type(old) is types.StringType:
            regexp = re.compile(old)
        else:
            regexp = old
            
        for index in range(len(self.lines)):
            line = self.lines[index]
            match = regexp.match(line)
            if match:
                line = '#>- ' + line
                newlines = string.split(new, '\n')
                self.lines[index:index+1] = [line, '#>+ %d' % len(newlines)] + newlines
                return

    def addTarget(self, target, dependson):
        if not self.target_adds.has_key(target):
            self.target_adds[target] = [dependson]
        else:
            self.target_adds[target].append(dependson)
            
    def appendLines(self, newlines):
        lines = string.split(newlines, '\n') + ['\n']
        self.lines.extend(['#>+ %d' % len(lines)] + lines)
        
    def restore(self):
        index = 0
        while index < len(self.lines):
            line = self.lines[index]
            if line[0:3] == '#>+':
                # the +1 is the comment itself
                linec = string.atoi(line[3:]) + 1
                del self.lines[index:index+linec]
                continue
            if line[0:3] == '#>-':
                self.lines[index] = self.lines[index][4:]
            index = index + 1
        
    def initialize(self):
        global foreigndirs

        os.chdir(self.makefileDir)
        self.printname = string.replace(self.makefile, topdir, "")
        self.makefile = os.path.basename(self.makefile)

        if not posixpath.exists("Makefile.am"):
            raise self.NoMakefileAmFound, self.makefileDir

        for dir in foreigndirs:
            if dir.match(self.makefileDir):
                print 'leaving ' + self.makefileDir
                return 0

        f = open(self.makefile)
        self.lines = []
      
        while 1:
            line = f.readline()
            if not line: break
            self.lines.append(string.rstrip(line))

        f.close()
        
        # take out the 
        self.restore()
        
        optionline = re.compile('^\s*(\w+)\s*=\s*([^\n]*)$')
        linecontinued = re.compile('\\\s*\n')
        lastline = ''

        index = 0
        while index < len(self.lines):
            line = self.lines[index]
            if linecontinued.search(line):
                self.lines[index] = linecontinued.sub(' ', line) + self.lines[index+1]
                continue
            else:
                index = index + 1

            match = optionline.search(line)
            if match:
                self.options[match.group(1)] = match.group(2)

        if self.options.has_key('KDE_OPTIONS'):
            options = string.split(self.options['KDE_OPTIONS'])
            if 'foreign' in options:
                foreigndirs.append(re.compile(self.makefileDir + "/.*"))
                return 0

        self.cxxsuffix = ""
        suffixes = re.compile('^\.SUFFIXES:(.*)$')

        for line in self.lines:
            match = suffixes.match(line)
            if match:
                existing_suffixes = string.split(match.group(1))
                for suffix in existing_suffixes:
                    # leave out the .
                    if suffix[1:] in cppsuffixes:
                        self.cxxsuffix = suffix[1:]
                        break
                if self.cxxsuffix:
                    break

        search_real_programs = {}

        for option in self.options.keys():
            if string.rfind(option, '_OBJECTS') > 0:

                program = option[0:string.find(option, '_OBJECTS')]
                objs = self.options[option]

                variable_in_objects = 0

                objlist = string.split(objs)
                variable = re.compile('\$\((\w+)\)')
                for obj in objlist:
                    match = variable.match(obj)
                    if match and not match.group(1) == 'OBJEXT':
                        variable_in_objects = 1
                        break

                if variable_in_objects:
                    continue

                if len(program) > 3 and program[3] == 'am_':
                    program = program[3:]

                if verbose:
                    print "found program " + program

                self.programs.append(program)
                self.realobjs[program] = objs

                if self.options.has_key(program + "_SOURCES"):
                    self.sources[program] = self.options[program + "_SOURCES"]
                else:
                    self.sources[program] = ""
                    sys.stderr.write("found program with no _SOURCES: " + program + '\n')

                # unmask to regexp
                realprogram = string.replace(program, '_', '.')
                search_real_programs[program] = re.compile('.*(' + realprogram +
                                                           ')(\$\(EXEEXT\)?)?:.*\$\(' +
                                                           program + '_OBJECTS\).*')

                self.realname[program] = "";

        for line in self.lines:
            if string.find(line, '_OBJECTS') > 0: # just a random piece to not use at _every_ line
                for program in self.programs:
                    match = search_real_programs[program].match(line)
                    if match:
                        self.realname[program] = match.group(1)

    def finalTouch(self):
        if self.options.has_key('DEPDIR'):
            sys.stderr.write(self.printname + " defines DEPDIR. This means you're using automake > 1.4 - this is not supported!")
        else:
            # taken out a random variable
            self.substituteLine('bindir\s*=.*', 'DEPDIR = .deps\nbindir = ' + self.options['bindir'])

        self.appendLines('cvs-clean:\n' +
                         '\t$(MAKE) -f $(top_srcdir)/admin/Makefile.common cvs-clean')

        self.appendLines('kde-rpo-clean:\n'+
                         '\t-rm -f *.rpo')

        self.addTarget('clean', 'kde-rpo-clean')
        self.addAllTargets()

    def addAllTargets(self):
        for target in self.target_adds.keys():
            match = self.findLine(target + ':\s*(.*)')
            if match:
                self.substituteLine(match.re, target + ': ' +
                                    string.join(self.target_adds[target]) +
                                    ' ' + match.group(1))
                    
    def writeback(self):
        f = open(self.makefile, 'w')
        for line in self.lines:
            f.write(line)
            f.write('\n')
        f.close()

    def tag_automake(self):
        match = self.findLine('^(.*cd \$\(top_srcdir\)\s+&&\s+\$\(AUTOMAKE\).*)$')
        if not match: return 1
        self.substituteLine(match.re, match.group(1) + '\n' +
                       '\tcd $(top_srcdir) && python ' +
                       thisProg + ' ' + self.printname)
        
def main():
    global use_final, dryrun, pathoption, thisProg, verbose

    optlist, makefiles = getopt.getopt(sys.argv[1:], 'vhp:n', [
        'version', 'verbose', 'path=', 'help', 'no-final'])

    for option, param in optlist:
        if option == '--version':
            print "\n"
            print thisProg + "$Revision$"
            print "This is really free software, unencumbered by the GPL."
            print "You can do anything you like with it except sueing me."
            print "Copyright 1998 Kalle Dalheimer <kalle\@kde.org>"
            print "Concept, design and unnecessary questions about perl"
            print "     by Matthias Ettrich <ettrich\@kde.org>"
            print ""
            print "Making it useful by Stephan Kulow <coolo\@kde.org> and"
            print "Harri Porten <porten\@kde.org>"
            print "Updated (Feb-1999), John Birch <jb.nz\@writeme.com>"
            print "Current Maintainer Stephan Kulow"
            sys.exit(0)
        if option == '--verbose' or option == '-v':
            verbose = 1
        if option == '-p' or option == '--path':
            thisProg = param + "/" + thisProg
            if (not posixpath.exists(thisProg)):
                sys.stderr.write(thisProg + " doesn't exist\n")
            pathoption=1
        if option == '--help' or option == '-h':
            print "Usage " + thisProg + " [OPTION] ... [dir/Makefile.in]..."
            print "Patches dir/Makefile.in generated from automake"
            print "(where dir can be a full or relative directory name)"
            print "  -v, --verbose      verbosely list files processed"
            print "  -h, --help         print this help, then exit"
            print "  --version          print version number, then exit"
            print "  -p, --path=        use the path to am_edit if the path"
            print "  --no-final         don't patch for --enable-final"
            print "                     called from is not the one to be used"
            sys.exit(0)
        if option == '--no-final':
            use_final = 0
        if option == '-n':
            dryrun = 1

    if not use_final:
        thisProg = thisProg + " --no-final"

    if thisProg[0] == '/' and not pathoption:
        sys.stderr.write( "Illegal full pathname call performed...\n"
                          "The call to \"" + thisProg + "\"\n"
                          "would be inserted in some Makefile.in.\n"
                          "Please use option --path.\n")
        sys.exit(1)

    if len(makefiles) == 0:
        import find
        makefiles = find.find('Makefile.in')

    for index in range(len(makefiles)):
        if not makefiles[index][0] == '/':
            makefiles[index] = os.path.normcase(os.path.abspath(makefiles[index]))

    makefiles.sort()
    for file in makefiles:
        makefile = Makefile(file)
        try:
            makefile.initialize()
            makefile.tag_automake()
            makefile.finalTouch()
            makefile.writeback()
        except Makefile.NoMakefileAmFound, param:
            if verbose: print Makefile.NoMakefileAmFound + ' in ' + param

main()
