;; fortunes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'comment-util)
(require 'common)
(require 'persistent-store)

(defvar *perlis-quotes*
  ["“Both knowledge and wisdom extend man’s reach. Knowledge led to
computers, wisdom to chopsticks.”
―Alan Perlis"

   "“There is an appreciated substance to the phrase ‘ALGOL-like’ which
is often used in arguments about programming, languages and
computation. ALGOL appears to be a durable model, and even flourishes
under surgery — be it explorative, plastic, or amputative.”
―Alan Perlis"

   "“The vision we have of conversational programming takes in much more
than rapid turn around time and convenient debugging aids: our most
interesting programs are never wrong and never final. [...] What is
new is the requirement to make variable in our languages what we had
previously taken as fixed. I do not refer to new data classes now, but
to variables whose values are programs or parts of programs, syntax or
parts of syntax, and regimes of control.”
―Alan Perlis"

   "“This language [LISP] induces humorous arguments among programmers,
often being damned and praised for the same feature.”
―Alan Perlis"

   "“Programmers should never be satisfied with languages which permit
them to program everything, but to program nothing of interest easily.”
―Alan Perlis"

   "“Computer science is a restless infant and its progress depends as
much on shifts in point of view as on the orderly development of our
current concepts.”
―Alan Perlis"

   "“Functions delay binding: data structures induce binding. Moral:
Structure data late in the programming process.”
―Alan Perlis, Epigrams on Programming"

   "“Syntactic sugar causes cancer of the semi-colons.”
―Alan Perlis, Epigrams on Programming"

   "“Every program is a part of some other program and rarely fits.”
―Alan Perlis, Epigrams on Programming"

   "“If a program manipulates a large amount of data, it does so in a
small number of ways.”
―Alan Perlis, Epigrams on Programming"

   "“Symmetry is a complexity reducing concept (co-routines include
sub-routines); seek it everywhere.”
―Alan Perlis, Epigrams on Programming"

   "“It is easier to write an incorrect program than understand a correct one.”
―Alan Perlis, Epigrams on Programming"

   "“A programming language is low level when its programs require
attention to the irrelevant.”
―Alan Perlis, Epigrams on Programming"

   "“It is better to have 100 functions operate on one data structure than
10 functions on 10 data structures.”
―Alan Perlis, Epigrams on Programming"

   "“Get into a rut early: Do the same processes the same way. Accumulate
idioms. Standardize. The only difference (!) between Shakespeare and
you was the size of his idiom list - not the size of his
vocabulary.”
―Alan Perlis, Epigrams on Programming"

   "“If you have a procedure with 10 parameters, you probably missed some.”
―Alan Perlis, Epigrams on Programming"

   "“Recursion is the root of computation since it trades description for time.”
―Alan Perlis, Epigrams on Programming"

   "“If two people write exactly the same program, each should be put in
micro-code and then they certainly won’t be the same.”
―Alan Perlis, Epigrams on Programming"

   "“In the long run every program becomes rococo - then rubble.”
―Alan Perlis, Epigrams on Programming"

   "“Everything should be built top-down, except the first time.”
―Alan Perlis, Epigrams on Programming"

   "“Every program has (at least) two purposes: the one for which it was
written and another for which it wasn’t.”
―Alan Perlis, Epigrams on Programming"

   "“If a listener nods his head when you’re explaining your program, wake him up.”
―Alan Perlis, Epigrams on Programming"

   "“A program without a loop and a structured variable isn’t worth writing.”
―Alan Perlis, Epigrams on Programming"

   "“A language that doesn’t affect the way you think about programming,
is not worth knowing.”
―Alan Perlis, Epigrams on Programming"

   "“Wherever there is modularity there is the potential for misunderstanding:
Hiding information implies a need to check communication.”
―Alan Perlis, Epigrams on Programming"

   "“Optimization hinders evolution.”
―Alan Perlis, Epigrams on Programming"

   "“A good system can’t have a weak command language.”
―Alan Perlis, Epigrams on Programming"

   "“To understand a program you must become both the machine and the program.”
―Alan Perlis, Epigrams on Programming"

   "“Perhaps if we wrote programs from childhood on, as adults we’d be
able to read them.”
―Alan Perlis, Epigrams on Programming"

   "“One can only display complex information in the mind. Like seeing,
movement or flow or alteration of view is more important than the
static picture, no matter how lovely.”
―Alan Perlis, Epigrams on Programming"

   "“There will always be things we wish to say in our programs that in
all known languages can only be said poorly.”
―Alan Perlis, Epigrams on Programming"

   "“Once you understand how to write a program get someone else to write it.”
―Alan Perlis, Epigrams on Programming"

   "“Around computers it is difficult to find the correct unit of time to
measure progress. Some cathedrals took a century to complete. Can you
imagine the grandeur and scope of a program that would take as
long?”
―Alan Perlis, Epigrams on Programming"

   "“For systems, the analogue of a face-lift is to add to the control
graph an edge that creates a cycle, not just an additional node.”
―Alan Perlis, Epigrams on Programming"

   "“In programming, everything we do is a special case of something more
general - and often we know it too quickly.”
―Alan Perlis, Epigrams on Programming"

   "“Simplicity does not precede complexity, but follows it.”
―Alan Perlis, Epigrams on Programming"

   "“Programmers are not to be measured by their ingenuity and their logic
but by the completeness of their case analysis.”
―Alan Perlis, Epigrams on Programming"

   "“The 11th commandment was ‘Thou Shalt Compute’ or ‘Thou Shalt Not
Compute’ - I forget which.”
―Alan Perlis, Epigrams on Programming"

   "“The string is a stark data structure and everywhere it is passed
there is much duplication of process. It is a perfect vehicle for
hiding information.”
―Alan Perlis, Epigrams on Programming"

   "“Everyone can be taught to sculpt: Michelangelo would have had to be
taught how not to. So it is with the great programmers.”
―Alan Perlis, Epigrams on Programming"

   "“The use of a program to prove the 4-color theorem will not change
mathematics - it merely demonstrates that the theorem, a challenge for
a century, is probably not important to mathematics.”
―Alan Perlis, Epigrams on Programming"

   "“The most important computer is the one that rages in our skulls and
ever seeks that satisfactory external emulator. The standardization of
real computers would be a disaster - and so it probably won’t
happen.”
―Alan Perlis, Epigrams on Programming"

   "“Structured Programming supports the law of the excluded muddle.”
―Alan Perlis, Epigrams on Programming"

   "“Re graphics: A picture is worth 10K words - but only those to
describe the picture. Hardly any sets of 10K words can be adequately
described with pictures.”
―Alan Perlis, Epigrams on Programming"

   "“There are two ways to write error-free programs; only the third one works.”
―Alan Perlis, Epigrams on Programming"

   "“Some programming languages manage to absorb change, but withstand progress.”
―Alan Perlis, Epigrams on Programming"

   "“You can measure a programmer’s perspective by noting his attitude on
the continuing vitality of FORTRAN.”
―Alan Perlis, Epigrams on Programming"

   "“In software systems it is often the early bird that makes the worm.”
―Alan Perlis, Epigrams on Programming"

   "“Sometimes I think the only universal in the computing
field is the fetch-execute-cycle.”
―Alan Perlis, Epigrams on Programming"

   "“The goal of computation is the emulation of our synthetic abilities,
not the understanding of our analytic ones.”
―Alan Perlis, Epigrams on Programming"

   "“Like punning, programming is a play on words.”
―Alan Perlis, Epigrams on Programming"

   "“As Will Rogers would have said, ‘There is no such thing as a free variable.’”
―Alan Perlis, Epigrams on Programming"

   "“The best book on programming for the layman is ‘Alice in
Wonderland’; but that’s because it’s the best book on anything for
the layman.”
―Alan Perlis, Epigrams on Programming"

   "“Giving up on assembly language was the apple in our Garden of Eden:
Languages whose use squanders machine cycles are sinful. The LISP
machine now permits LISP programmers to abandon bra and fig-leaf.”
―Alan Perlis, Epigrams on Programming"

   "“When we understand knowledge-based systems, it will be as before -
except our finger-tips will have been singed.”
―Alan Perlis, Epigrams on Programming"

   "“Bringing computers into the home won’t change either one, but may
revitalize the corner saloon.”
―Alan Perlis, Epigrams on Programming"

   "“Systems have sub-systems and sub-systems have sub-systems and so on
ad infinitum - which is why we’re always starting over.”
―Alan Perlis, Epigrams on Programming"

   "“So many good ideas are never heard from again once they embark in a
voyage on the semantic gulf.”
―Alan Perlis, Epigrams on Programming"

   "“Beware of the Turing tar-pit in which everything is possible but
nothing of interest is easy.”
―Alan Perlis, Epigrams on Programming"

   "“Software is under a constant tension. Being symbolic it is
arbitrarily perfectible; but also it is arbitrarily changeable.”
―Alan Perlis, Epigrams on Programming"

   "“It is easier to change the specification to fit the program than vice versa.”
―Alan Perlis, Epigrams on Programming"

   "“Fools ignore complexity. Pragmatists suffer it. Some can avoid it.
Geniuses remove it.”
―Alan Perlis, Epigrams on Programming"

   "“In English every word can be verbed. Would that it were so in our
programming languages.”
―Alan Perlis, Epigrams on Programming"

   "“Dana Scott is the Church of the Lattice-Way Saints.”
―Alan Perlis, Epigrams on Programming"

   "“In programming, as in everything else, to be in error is to be reborn.”
―Alan Perlis, Epigrams on Programming"

   "“In computing, invariants are ephemeral.”
―Alan Perlis, Epigrams on Programming"

   "“When we write programs that ‘learn’, it turns out we do and they don’t.”
―Alan Perlis, Epigrams on Programming"

   "“Often it is means that justify ends: Goals advance technique and
technique survives even when goal structures crumble.”
―Alan Perlis, Epigrams on Programming"

   "“Make no mistake about it: Computers process numbers - not symbols. We
measure our understanding (and control) by the extent to which we can
arithmetize an activity.”
―Alan Perlis, Epigrams on Programming"

   "“Making something variable is easy. Controlling duration of constancy
is the trick.”
―Alan Perlis, Epigrams on Programming"

   "“Think of all the psychic energy expended in seeking a fundamental
distinction between ‘algorithm’ and ‘program’.”
―Alan Perlis, Epigrams on Programming"

   "“If we believe in data structures, we must believe in independent
(hence simultaneous) processing. For why else would we collect items
within a structure? Why do we tolerate languages that give us the one
without the other?”
―Alan Perlis, Epigrams on Programming"

   "“In a 5 year period we get one superb programming language. Only we
can’t control when the 5 year period will begin.”
―Alan Perlis, Epigrams on Programming"

   "“Over the centuries the Indians developed sign language for
communicating phenomena of interest. Programmers from different tribes
(FORTRAN, LISP, ALGOL, SNOBOL, etc.) could use one that doesn’t
require them to carry a blackboard on their ponies.”
―Alan Perlis, Epigrams on Programming"

   "“Documentation is like term insurance: It satisfies because almost no
one who subscribes to it depends on its benefits.”
―Alan Perlis, Epigrams on Programming"

   "“An adequate bootstrap is a contradiction in terms.”
―Alan Perlis, Epigrams on Programming"

   "“It is not a language’s weaknesses but its strengths that control the
gradient of its change: Alas, a language never escapes its embryonic sac.”
―Alan Perlis, Epigrams on Programming"

   "“It is possible that software is not like anything else, that it is
meant to be discarded: that the whole point is to always see it as
soap bubble?”
―Alan Perlis, Epigrams on Programming"

   "“Because of its vitality, the computing field is always in desperate
need of new cliches: Banality soothes our nerves.”
―Alan Perlis, Epigrams on Programming"

   "“It is the user who should parameterize procedures, not their
creators.”
―Alan Perlis, Epigrams on Programming"

   "“The cybernetic exchange between man, computer and algorithm is like a
game of musical chairs: The frantic search for balance always leaves
one of the three standing ill at ease.”
―Alan Perlis, Epigrams on Programming"

   "“If your computer speaks English it was probably made in Japan.”
―Alan Perlis, Epigrams on Programming"

   "“A year spent in artificial intelligence is enough to
make one believe in God.”
―Alan Perlis, Epigrams on Programming"

   "“Prolonged contact with the computer turns mathematicians into clerks
and vice versa.”
―Alan Perlis, Epigrams on Programming"

   "“In computing, turning the obvious into the useful is a living
definition of the word ‘frustration’.”
―Alan Perlis, Epigrams on Programming"

   "“We are on the verge: Today our program proved Fermat’s next-to-last theorem!”
―Alan Perlis, Epigrams on Programming"

   "“What is the difference between a Turing machine and the modern
computer? It’s the same as that between Hillary’s ascent of Everest
and the establishment of a Hilton hotel on its peak.”
―Alan Perlis, Epigrams on Programming"

   "“Motto for a research laboratory: What we work on today, others will
first think of tomorrow.”
―Alan Perlis, Epigrams on Programming"

   "“Though the Chinese should adore APL, it’s FORTRAN they put their money on.”
―Alan Perlis, Epigrams on Programming"

   "“We kid ourselves if we think that the ratio of procedure to data in
an active data-base system can be made arbitrarily small or even kept small.”
―Alan Perlis, Epigrams on Programming"

   "“We have the mini and the micro computer. In what semantic niche would
the pico computer fall?”
―Alan Perlis, Epigrams on Programming"

   "“It is not the computer’s fault that Maxwell’s equations are not
adequate to design the electric motor.”
―Alan Perlis, Epigrams on Programming"

   "“One does not learn computing by using a hand calculator, but one can
forget arithmetic.”
―Alan Perlis, Epigrams on Programming"

   "“Computation has made the tree flower.”
―Alan Perlis, Epigrams on Programming"

   "“The computer reminds one of Lon Chaney - it is the machine of a
thousand faces.”
―Alan Perlis, Epigrams on Programming"

   "“The computer is the ultimate polluter. Its feces are
indistinguishable from the food it produces.”
―Alan Perlis, Epigrams on Programming"

   "“When someone says ‘I want a programming language in which I need
only say what I wish done,’ give him a lollipop.”
―Alan Perlis, Epigrams on Programming"

   "“Interfaces keep things tidy, but don’t accelerate growth: Functions do.”
―Alan Perlis, Epigrams on Programming"

   "“Don’t have good ideas if you aren’t willing to be responsible for them.”
―Alan Perlis, Epigrams on Programming"

   "“Computers don’t introduce order anywhere as much as they expose opportunities.”
―Alan Perlis, Epigrams on Programming"

   "“When a professor insists computer science is X but not Y, have
compassion for his graduate students.”
―Alan Perlis, Epigrams on Programming"

   "“In computing, the mean time to failure keeps getting shorter.”
―Alan Perlis, Epigrams on Programming"

   "“In man-machine symbiosis, it is man who must adjust: The machines can’t.”
―Alan Perlis, Epigrams on Programming"

   "“We will never run out of things to program as long as there is a
single program around.”
―Alan Perlis, Epigrams on Programming"

   "“Dealing with failure is easy: Work hard to improve. Success is also
easy to handle: You’ve solved the wrong problem. Work hard to improve.”
―Alan Perlis, Epigrams on Programming"

   "“One can’t proceed from the informal to the formal by formal means.”
―Alan Perlis, Epigrams on Programming"

   "“Purely applicative languages are poorly applicable.”
―Alan Perlis, Epigrams on Programming"

   "“The proof of a system’s value is its existence.”
―Alan Perlis, Epigrams on Programming"

   "“You can’t communicate complexity, only an awareness of it.”
―Alan Perlis, Epigrams on Programming"

   "“It’s difficult to extract sense from strings, but they’re the only
communication coin we can count on.”
―Alan Perlis, Epigrams on Programming"

   "“The debate rages on: Is PL/I Bactrian or Dromedary?”
―Alan Perlis, Epigrams on Programming"

   "“Whenever two programmers meet to criticize their programs, both are silent.”
―Alan Perlis, Epigrams on Programming"

   "“Think of it! With VLSI we can pack 100 ENIACs in 1 sq.cm.”
―Alan Perlis, Epigrams on Programming"

   "“Editing is a rewording activity.”
―Alan Perlis, Epigrams on Programming"

   "“Why did the Roman Empire collapse? What is the Latin for office automation?”
―Alan Perlis, Epigrams on Programming"

   "“Computer Science is embarrassed by the computer.”
―Alan Perlis, Epigrams on Programming"

   "“The only constructive theory connecting neuroscience and psychology
will arise from the study of software.”
―Alan Perlis, Epigrams on Programming"

   "“Within a computer natural language is unnatural.”
―Alan Perlis, Epigrams on Programming"

   "“Most people find the concept of programming obvious, but the doing impossible.”
―Alan Perlis, Epigrams on Programming"

   "“You think you know when you learn, are more sure when you can write,
even more when you can teach, but certain when you can
program.”
―Alan Perlis, Epigrams on Programming"

   "“It goes against the grain of modern education to teach children to
program. What fun is there in making plans, acquiring discipline in
organizing thoughts, devoting attention to detail and learning to be
self-critical?”
―Alan Perlis, Epigrams on Programming"

   "“If you can imagine a society in which the computer-robot is the only
menial, you can imagine anything.”
―Alan Perlis, Epigrams on Programming"

   "“Programming is an unnatural act.”
―Alan Perlis, Epigrams on Programming"

   "“Adapting old programs to fit new machines usually means adapting new
machines to behave like old ones.”
―Alan Perlis, Epigrams on Programming"

   "“In seeking the unattainable, simplicity only gets in the way.”
―Alan Perlis, Epigrams on Programming"

   "“If there are epigrams, there must be meta-epigrams.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams are interfaces across which appreciation and insight flow.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams parameterize auras.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams are macros, since they are executed at read time.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams crystallize incongruities.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams retrieve deep semantics from a data base that is all procedure.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams scorn detail and make a point:
They are a superb high-level documentation.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams are more like vitamins than protein.”
―Alan Perlis, Epigrams on Programming"

   "“Epigrams have extremely low entropy.”
―Alan Perlis, Epigrams on Programming"

   "“The last epigram? Neither eat nor drink them, snuff epigrams.”
―Alan Perlis, Epigrams on Programming"
   ]
  "Quotes and epigrams by Alan J. Perlis")

(defvar *good-fortunes*
  ["“γνωθι σεαUτόυ \(Познай себя\)”
―Inscription at Temple of Apollo at Delphi"

   "“Non est salvatori salvator,
neque defensori dominus,
nec pater nec mater,
nihil supernum.”
―Godric Gryffindor, 1202 A.D."

   "“Everything always takes twice as long and costs four times
as much as you planned.”
―programmer’s axiom"

   "“To those accustomed to the precise, structured methods of conventional
system development, exploratory development techniques may seem messy,
inelegant, and unsatisfying.  But it’s a question of congruence:
precision and flexibility may be just as disfunctional in novel,
uncertain situations as sloppiness and vacillation are in familiar,
well-defined ones.  Those who admire the massive, rigid bone structures
of dinosaurs should remember that jellyfish still enjoy their very
secure ecological niche.”
―Beau Sheil, “Power Tools for Programmers”"

   "“Putting twice as many programmers on a project that is late
will make it twice as late.”
―Brooks’ law of programming projects"

   "“At some time in the project you’re going to have to break down and
finally define the problem.”
―programmer’s saying"

   "“1 Thou shalt run lint frequently and study its pronouncements with
care, for verily its perception and judgement oft exceed thine.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“2 Thou shalt not follow the NULL pointer, for chaos and madness await
thee at its end.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“3 Thou shalt cast all function arguments to the expected type if they
are not of that type already, even when thou art convinced that this
is unnecessary, lest they take cruel vengeance upon thee when thou
least expect it.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“4 If thy header files fail to declare the return types of thy library
functions, thou shalt declare them thyself with the most meticulous
care, lest grievous harm befall thy program.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“5 Thou shalt check the array bounds of all strings (indeed, all
arrays), for surely where thou typest ‘foo’ someone someday shall
type ‘supercalifragilistic-expialidocious’.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“6 If a function be advertised to return an error code in the event of
difficulties, thou shalt check for that code, yea, even though the
checks triple the size of thy code and produce aches in thy typing
fingers, for if thou thinkest ‘it cannot happen to me’, the gods
shall surely punish thee for thy arrogance.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“7 Thou shalt study thy libraries and strive not to re-invent them
without cause, that thy code may be short and readable and thy days
pleasant and productive.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“8 Thou shalt make thy program’s purpose and structure clear to thy
fellow man by using the One True Brace Style, even if thou likest it
not, for thy creativity is better used in solving problems than in
creating beautiful new impediments to understanding.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“9 Thy external identifiers shall be unique in the first six
characters, though this harsh discipline be irksome and the years of
its necessity stretch before thee seemingly without end, lest thou
tear thy hair out and go mad on that fateful day when thou desirest to
make thy program run on an old system.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“10 Thou shalt foreswear, renounce, and abjure the vile heresy which
claimeth that “All the world’s a VAX”, and have no commerce with the
benighted heathens who cling to this barbarous belief, that the days
of thy program may be long even though the days of thy current machine
be short.”
―Henry Spencer, The Ten Commandments for C Programmers"

   "“After all is said and done, more is said than done.”
―Aesop"

   "“When I was born, I was so surprised I didn’t talk for a year and a half.”
―Gracie Allen"

   "“Love is not about who you live with. It’s about who you can’t live without.”
―Anonymous"

   "“Education is the best provision for the journey to old age.”
―Aristotle"

   "“One swallow does not make the spring.”
―Aristotle"

   "“Pleasure in the job puts perfection in the work.”
―Aristotle"

   "“We are what we repeatedly do.”
―Aristotle"

   "“Wishing to be friends is quick work, but friendship is a slow ripening fruit.”
―Aristotle"

   "“The worst solitude is to be destitute of sincere friendship.”
―Sir Francis Bacon"

   "“Knowledge is power.”
―Sir Francis Bacon"

   "“An error doesn’t become a mistake until you refuse to correct it.”
―Orlando A. Battista"

   "“When one door closes, another opens; but we often look so long
and so regretfully upon the closed door that we do not see the one
which has opened for us.”
―Alexander Graham Bell"

   "“Happiness lies in good health and a bad memory.”
―Ingrid Bergman"

   "“Ability is nothing without opportunity.”
―Napoleon Bonaparte"

   "“Never interrupt your enemy when he is making a mistake.”
―Napoleon Bonaparte"

   "“The heart has reasons that reason does not understand.”
―Jacques Bossuet"

   "“Never stand begging for that which you have the power to earn.”
―Miguel de Cervantes"

   "“There are people who have money and people who are rich.”
―Coco Chanel"

   "“Don’t ever take a fence down until you know the reason it was put up.”
―G. K. Chesterton"

   "“The price of greatness is responsibility.”
―Winston Churchill"

   "“We make a living by what we get.  We make a life by what we give.”
―Winston Churchill"

   "“A pessimist sees the difficulty in every opportunity;
an optimist sees the opportunity in every difficulty.”
―Winston Churchill"

   "“Success is the ability to go from one failure to another with
no loss of enthusiasm.”
―Winston Churchill"

   "“If you are going through hell, keep going.”
―Winston Churchill"

   "“A lie gets halfway around the world before the truth has a
chance to get its pants on.”
―Winston Churchill"

   "“History will be kind to me for I intend to write it.”
―Winston Churchill"

   "“Gratitude is not only the greatest of virtues, but the parent of all others.”
―Cicero"

   "“Our greatest glory is not in never falling, but in rising every time we fall.”
―Conficius"

   "“Respect yourself and others will respect you.”
―Conficius"

   "“Nothing in life is to be feared; it is only to be understood.”
―Marie Curie"

   "“Have no fear of perfection;  you’ll never reach it.”
―Salvador Dali"

   "“It is not the strongest of the species that survives, nor the most intelligent,
but the one most responsive to change.”
―Charles Darwin"

   "“The greatest good you can do for another is not just share your riches,
but reveal to him his own.”
―Benjamin Disraeli"

   "“Pure love and suspicion cannot dwell together:
at the door where the latter enters, the former makes its exit.”
―Alexandre Dumas"

   "“Genius is one percent inspiration and ninety-nine percent perspiration.”
―Thomas Edison"

   "“Only two things are infinite - the universe and human stupidity,
and I’m not sure about the former.”
―Albert Einstein"

   "“Not everything that can be counted counts,
and not everything that counts can be counted.”
―Albert Einstein"

   "“The difference between genius and stupidity is that genius has its limits.”
―Albert Einstein"

   "“It’s not that I’m so smart, it’s just that I stay with problems longer.”
―Albert Einstein"

   "“Anyone who has never made a mistake has never tried anything new.”
―Albert Einstein"

   "“Great spirits have always encountered violent opposition from mediocre minds.”
―Albert Einstein"

   "“I never think of the future.  It comes soon enough.”
―Albert Einstein"

   "“In case of dissension, never dare to judge till you’ve heard the other side.”
―Euripides"

   "“Man’s best possession is a sympathetic wife.”
―Euripides"

   "“A writer is congenitally unable to tell the truth,
and that is why we call what he writes fiction.”
―William Faulkner"

   "“Obstacles are those frightful things you see when
you take your eyes off your goal.”
―Henry Ford"

   "“You can’t build a reputation on what you are going to do.”
―Henry Ford"

   "“The biggest mistake people make in life is not trying to make a living
at doing what they most enjoy.”
―Malcolm Forbes"

   "“To accomplish great things, we must not only act, but also dream;
not only plan but also believe.”
―Anatole France"

   "“Most fools think they are only ignorant.”
―Benjamin Franklin"

   "“Blessed is he who expects nothing, for he shall never be disappointed.”
―Benjamin Franklin"

   "“Creative minds have always been known to survive any kind of bad training.”
―Anna Freud"

   "“You must be the change you wish to see in the world.”
―Mahatma Gandhi"

   "“Live as if you were to die tomorrow; learn as if you were to live forever.”
―Mahatma Gandhi"

   "“If you can actually count your money, then you’re not a rich man.”
―J. Paul Getty"

   "“Enjoy when you can and endure when you must.”
―Johann Wolfgang van Goethe"

   "“A man can stand anything except a succession of ordinary days.”
―Johann Wolfgang van Goethe"

   "“Talent develops in tranquillity, character in the full current of human life.”
―Johann Wolfgang van Goethe"

   "“Never confuse movement with action.”
―Ernest Hemmingway"

   "“Drama is life with the dull bits cut out.”
―Alfred Hitchcock"

   "“Forty is the old age of youth;  fifty is the youth of old age.”
―Victor Hugo"

   "“Life is the flower for which love is the honey.”
―Victor Hugo"

   "“Intelligence is the wife, imagination is the mistress, memory is the servant.”
―Victor Hugo"

   "“Our brightest blazes are commonly kindled by unexpected sparks.”
―Samuel Johnson"

   "“When making your choices in life, do not forget to live.”
―Samuel Johnson"

   "“Love is the wisdom of the fool and the folly of the wise.”
―Samuel Johnson"

   "“Science is organized knowledge.  Wisdom is organized life.”
―Immanuel Kant"

   "“Forgive your enemies, but never forget their names.”
―John F. Kennedy"

   "“When written in Chinese, the word crisis is composed of two characters.
One represents danger, the other represents opportunity.”
―John F. Kennedy"

   "“Liberty without learning is always in peril;
learning without liberty is always in vain.”
―John F. Kennedy"

   "“Take everything you like seriously, except yourselves.”
―Rudyard Kipling"

   "“Gardens are not made by sitting in the shade.”
―Rudyard Kipling"

   "“We can live without religion and meditation,
but we cannot survive without human affection.”
―Dalai Lama"

   "“Life is what happens to you when you are busy making other plans.”
―John Lennon"

   "“In the end it’s not the years in your life that count;
it’s the life in your years.”
―Abraham Lincoln"

   "“Nearly all men can stand adversity, but if you want to test a man’s character,
give him power.”
―Abraham Lincoln"

   "“You cannot escape the responsibility of tomorrow by evading it today.”
―Abraham Lincoln"

   "“You can fool all of the people some of the time,
some of the people all of the time,
but you can’t fool all of the people all of the time.”
―Abraham Lincoln"

   "“No enterprise is more likely to succeed than one concealed from the enemy
until it is ripe for execution.”
―Niccolo Machiavelli"

   "“To the soul, there is hardly anything more healing than friendship.”
―Thomas Moore"

   "“Tact is the knack of making a point without making an enemy.”
―Isaac Newton"

   "“The advantage of a bad memory is that one enjoys several times the same
good things for the first time.”
―Friedrich Nietzsche"

   "“What doesn’t kill you will make you stronger.”
―Friedrich Nietzche"

   "“When one has much to put into them, a day has a hundred pockets.”
―Friedrich Nietzsche"

   "“We don’t see things as they are; we see things as we are.”
―Anais Nin"

   "“Burdens become light when cheerfully borne.”
―Ovid"

   "“In the field of observation, chance favours only the prepared mind.”
―Louis Pasteur"

   "“The chief enemy of creativity is good taste.”
―Pablo Picasso"

   "“The mind is not a vessel to be filled, but a fire to be kindled.”
―Plutarch"

   "“The voyage to discovery is not in seeking new landscapes but
in having new eyes.”
―Marcel Proust"

   "“If you wish to avoid seeing a fool, you must break your mirror.”
―Francois Rabelais"

   "“Instruction ends in the schoolroom, but education ends only with life.”
―F.W. Robertson"

   "“The only thing we have to fear is fear itself.”
―Franklin D. Roosevelt"

   "“The only man who never makes mistakes is the man who never does anything.”
―Theodore Roosevelt"

   "“If it can’t be cured, it must be endured.”
―Salman Rushdie."

   "“When love and skill work together, expect a masterpiece.”
―John Ruskin"

   "“Life has taught us that love does not consist in gazing at each other,
but in looking outward together in the same direction.”
―Antoine de Saint Exupery"

   "“Hell is other people.”
―Jean-Paul Sartre"

   "“It is wise to learn; it is God-like to create.”
―John Saxe"

   "“Luck is what happens when preparation meets opportunity.”
―Lucius Annaeus Seneca"

   "“Life is a play. It’s not its length, but its performance that counts.”
―Lucius Annaeus Seneca"

   "“Action is eloquence.”
―William Shakespeare"

   "“All that glitters is not gold.”
―William Shakespeare"

   "“Our bodies are our gardens to which our wills are gardeners.”
―William Shakespeare"

   "“Great Britain and the United States are nations separated by a common language.”
―George Bernard Shaw"

   "“The greatest of our evils and the worst of our crimes is poverty.”
―George Bernard Shaw"

   "“Youth is a wonderful thing.  What a crime to waste it on children!”
―George Bernard Shaw"

   "“Martyrdom ... is the only way in which a man can become famous without ability.”
―George Bernard Shaw"

   "“We don’t stop playing because we grow old; we grow old because we stop playing.”
―George Bernard Shaw"

   "“Reasonable men adapt to the world.
Unreasonable men adapt the world to themselves.
That’s why all progress depends on unreasonable men.”
―George Bernard Shaw"

   "“Can a man who is warm understand one who is freezing?”
―Alexander Solzhenitsyn"

   "“Don’t judge each day by the harvest you reap ... but by the seeds you plant!”
―Robert Louis Stevenson"

   "“Politics is perhaps the only profession for which no preparation is
thought necessary.”
―Robert Louis Stevenson"

   "“Vision is the art of seeing things invisible.”
―Jonathan Swift"

   "“May you live every day of your life.”
―Jonathan Swift"

   "“I am more afraid of an army of a hundred sheep led by a lion,
than an army of a hundred lions led by a sheep.”
―Talleyrand"

   "“Tis better to have loved and lost than never to have loved at all.”
―Alfred Lord Tennyson"

   "“A good laugh is sunshine in a house.”
―William Thackery"

   "“Love is real only when a person can sacrifice himself for another.”
―Leo Tolstoy"

   "“It is better to keep your mouth closed and let people think you are a fool
than to open it and remove all doubt.”
―Mark Twain"

   "“Be careful about reading health books.  You may die of a misprint.”
―Mark Twain"

   "“Wrinkles should merely indicate where smiles have been.”
―Mark Twain"

   "“Remember, no one can make you feel inferior, without your consent.”
―Unknown"

   "“Genius is the ability to reduce the complicated to the simple.”
―Unknown"

   "“Only those who keep trying eventually win.”
―Unknown"

   "“Getting something done is an accomplishment;
getting something done right is an achievement.”
―Unknown"

   "“The best time to do something worthwhile is between yesterday and tomorrow.”
―Unknown"

   "“The progress of rivers to the ocean is not so rapid as that of man to error.”
―Voltaire"

   "“God is a comedian playing to an audience too afraid to laugh.”
―Voltaire"

   "“Experience is the name so many people give to their mistakes.”
―Oscar Wilde"

   "“Wisdom is knowing how little we know.”
―Oscar Wilde"

   "“To live is the rarest thing in the world.  Most people exist, that’s all.”
―Oscar Wilde"

   "“The old believe everything, the middle-aged suspect everything,
the young know everything.”
―Oscar Wilde"

   "“No man is rich enough to buy back his past.”
―Oscar Wilde"

   "“There are only two tragedies in life: one is not getting what one wants,
and the other is getting it.”
―Oscar Wilde"

   "“True friends stab you in the front.”
―Oscar Wilde"

   "“Fashion is a form of ugliness so intolerable that we have to alter it
every six months.”
―Oscar Wilde"

   "“The artist is nothing without the gift, but the gift is nothing without work.”
―Emile Zola"

   "“Data and procedures and the values they amass,
Higher-order functions to combine and mix and match,
Objects with their local state, the messages they pass,
A property, a package, the control point for a catch-
In the Lambda Order they are all first-class.
One Thing to name them all, One Thing to define them,
One Thing to place them in environments and bind them,
In the Lambda Order they are all first-class.”
―The TI PC Scheme Manual"

   "If your thesis is utter vacuous
Use first-order predicate calculus.
        With sufficient formality
        The sheerist banality
Will be hailed by the critics: ‘Miraculous!’"

   "“It is impossible to sharpen a pencil with a blunt axe.
It is equally vain to try to do it with ten blunt axes instead.”
―Edsger W. Dijkstra"

   "“The question of whether a computer can think is no more
interesting than the question of whether a submarine can swim.”
―Edsger W. Dijkstra"

   ;; "Nobody can fix the economy.  Nobody can be trusted with their finger on the
;; button.  Nobody’s perfect.  VOTE FOR NOBODY."

   "“One of the things every sorcerer will tell you, is that if you have
the name of a spirit, you have power over it.”
―Gerald Jay Sussman"

   "“There’s no sense in being precise, when you don’t even know what
you’re talking about.”
―John von Neumann"

   "“Programs must be written for people to read, and only incidentally
for machines to execute.”
―Abelson & Sussman, SICP \(preface to the first edition\)"

   "“One can even conjecture that Lisp owes its survival specifically to
the fact that its programs are lists, which everyone, including me,
has regarded as a disadvantage.”
―John McCarthy, “Early History of Lisp”"

   "“Man did not weave the web of life;
he is merely a strand in it.
Whatever he does to the web, he does to himself.”
                                  Seattle (c.1786-1866)
                                                 [1854]"

   "“Lasciate ogni speranza voi ch’entrate!  [Abandon all
hope, you who enter!]”
                                Dante Alighieri (1265-1321)
       _La Divina Commedia [The Divine Comedy]_ (1310-1321)
                                    “Inferno” canto 3, l. 1"

   "“When a distinguished but elderly scientist states that something is
possible, he is almost certainly right.  When he states that something
is impossible, he is very probably wrong.”
                             _Profiles of the Future_ (1962; rev. 1973)
                      “Hazards of Prophecy: The Failure of Imagination”
                                                     Clarke’s First Law"

   "“Perhaps the adjective ‘elderly’ requires definition.  In physics,
mathematics, and astronautics it means over thirty; in the other
disciplines, senile decay is sometimes postponed to the forties.  There
are, of course, glorious exceptions; but as every researcher just out
of college knows, scientists of over fifty are good for nothing but
board meetings, and should at all costs be kept out of the laboratory!”
                             _Profiles of the Future_ (1962; rev. 1973)
                      “Hazards of Prophecy: The Failure of Imagination”"

   "“But the only way of discovering the limits of the possible is to
venture a little way past them into the impossible.”
                             _Profiles of the Future_ (1962; rev. 1973)
                      “Hazards of Prophecy: The Failure of Imagination”
                                                    Clarke’s Second Law"

   "“When, however, the lay public rallies round an idea that is
denounced by distinguished but elderly scientists and supports that
idea with great fervor and emotion — the distinguished but elderly
scientists are then, after all, probably right.”
                                               Isaac Asimov (1920-1992)
                            _Fantasy & Science Fiction_ 1977 [magazine]
                                        In answer to Clarke’s First Law"

   "“Then indecision brings its own delays,
And days are lost lamenting o’er lost days.
Are you in earnest?  Seize this very minute;
What you can do, or dream you can, begin it;
Boldness has genius, power and magic in it.”
―Johann Wolfgang von Goethe \"Faust\""

   "“When you don’t create things, you become defined by your tastes
rather than ability. Your tastes only narrow & exclude people.
So create.”
―why the lucky stiff"

   "“Things should be made as simple as possible, but no simpler.”
―Albert Einstein"

   "“Our lives are frittered away by detail; simplify, simplify.”
―Henry David Thoreau"

   "“You can always recognize truth by its beauty and simplicity.”
―Richard Feynman"

   "“A vocabulary of truth and simplicity will be of service
throughout your life.”
―Winston Churchill"

   "“As you simplify your life, the laws of the universe will be simpler;
solitude will not be solitude, poverty will not be poverty,
nor weakness weakness.”
―Henry David Thoreau"

   "“Simplicity is the final achievement.
After one has played a vast quantity of notes and more notes,
it is simplicity that emerges as the crowning reward of art.”
―Frederic Chopin"

   "“Perfection is achieved, not when there is nothing more to add,
but when there is nothing left to take away.”
―Antoine de Saint Exupéry"

   "“Nothing is true, but that which is simple.”
―Johann Wolfgang von Goethe"

   "“Simplicity is the glory of expression.”
―Walt Whitman"

   "[As a new professor] at Cornell, I’d work on preparing my courses, and I’d
go over to the library a lot and read through the Arabian Nights and ogle
the girls that would go by. But when it came time to do some research,
I couldn’t get to work. I was a little tired; I was not interested; I
couldn’t do research! This went on for what I felt was a few years ...
I simply couldn’t get started on any problem: I remember writing one or
two sentences about some problem in gamma rays and then I couldn’t go
any further. I was convinced that from the war and everything else
(the death of my wife) I had simply burned myself out.

... Then I had another thought: Physics disgusts me a little bit now,
but I used to enjoy doing physics. Why did I enjoy it? I used to play
with it. I used to do whatever I felt like doing -- it didn’t have to
do with whether it was important for the development of nuclear physics,
but whether it was interesting and amusing for me to play with.
When I was in high school, I’d see water running out of a faucet growing
narrower, and wonder if I could figure out what determines that curve.
I found it was rather easy to do. I didn’t have to do it; it wasn’t
important for the future of science; somebody else had already done it.
That didn’t make any difference: I’d invent things and play with things
for my own entertainment.

So I got this new attitude. Now that I am burned out and I’ll never
accomplish anything, I’ve got this nice position at the university
teaching classes which I rather enjoy, and just like I read the
Arabian Nights for pleasure, I’m going to play with physics,
whenever I want to, without worrying about any importance whatsoever.

Within a week I was in the cafeteria and some guy, fooling around, throws a
plate in the air. As the plate went up in the air I saw it wobble, and I
noticed the red medallion of Cornell on the plate going around. It was pretty
obvious to me that the medallion went around faster than the wobbling.

I had nothing to do, so I start to figure out the motion of the rotating
plate. I discover that when the angle is very slight, the medallion rotates
twice as fast as the wobble rate -- two to one. ...

I went on to work out equations of wobbles. Then I thought about how
electron orbits start to move in relativity. Then there’s the Dirac Equation
in electrodynamics. And then quantum electrodynamics. And before I knew it
\(it was a very short time\) I was \"playing\" -- working, really -- with the
same old problem that I loved so much, that I had stopped working on when I
went to Los Alamos: my thesis-type problems; all those old-fashioned,
wonderful things.

It was effortless. It was easy to play with these things. It was like
uncorking a bottle: Everything flowed out effortlessly. I almost tried to
resist it! There was no importance to what I was doing, but ultimately
there was. The diagrams and the whole business that I got the Nobel Prize
for came from that piddling around with the wobbling plate.

Richard Feynman
Surely You’re Joking, Mr. Feynman, p 92"

   "“Coming back to where you started is not the same as never leaving.”
―Terry Pratchett"

   "“The Goal of a Teacher - To create an environment where learning happens.”
―Dr. George F. Corliss"

   "“Let us change our traditional attitude to the construction of programs:
Instead of imagining that our main task is to instruct a computer what to do,
let us concentrate rather on explaining to human beings what we want a
computer to do.”
―Donald Knuth"

   "“Never, never, never give up.”
―Winston Churchill"

   "“Be kind, for everyone you meet is fighting a hard battle.”
―Plato"

   "“Никто не должен быть жертвой собственной биографии.”
―Джордж Келли"

   "“Я не то, что со мной случилось, я — то, чем я решил стать.”
―Карл Густав Юнг"

   "“Но, боль превозмогая, с края пучины серной,
Возопил он к бойцам, недвижно лежащим,
Как листва устилает пластами лесные ручьи Валамброза,
Текущие мирно под сенью мрачных дубрав Этрурийских.”
―Мильтон, «Потеряный рай»"

   "“Ведь совесть — слово, созданное трусом,
Чтоб сильных напугать и остеречь.
Кулак нам — совесть, а закон нам — меч”
―Шекспир \(V, 3. Пер. А. Радловой\)"

   "NOTE: No warranties, either express or implied, are hereby given. All
software is supplied as is, without guarantee.  The user assumes all
responsibility for damages resulting from the use of these features,
including, but not limited to, frustration, disgust, system abends, disk
head-crashes, general malfeasance, floods, fires, shark attack, nerve
gas, locust infestation, cyclones, hurricanes, tsunamis, local
electromagnetic disruptions, hydraulic brake system failure, invasion,
hashing collisions, normal wear and tear of friction surfaces, cosmic
radiation, inadvertent destruction of sensitive electronic components,
windstorms, the Riders of Nazgul, infuriated chickens, malfunctioning
mechanical or electrical sexual devices, premature activation of the
distant early warning system, peasant uprisings, halitosis, artillery
bombardment, explosions, cave-ins, and/or frogs falling from the sky."

   "“A distributed system is one in which the failure of a computer you didn’t
even know existed can render your own computer unusable.”
―Leslie Lamport \(CACM, June 1992\)"

   "“A fractal is by definition a set for which the Hausdorff Besicovitch
dimension strictly exceeds the topological dimension.”
―Mandelbrot, The Fractal Geometry of Nature"

   "“Every man takes the limits of his own field of vision for the limits
of the world.”
―Schopenhauer"

   "“If that makes any sense to you, you have a big problem.”
―C. Durance, Computer Science 234"

   "“No man ever steps in the same river twice, for it’s not the
same river and he’s not the same man.”
―Heraclitas"

   "“We should be careful to get out of an experience only the wisdom that is
in it-and stop there; lest we be like the cat that sits down on a hot
stove-lid.  She will never sit down on a hot stove-lid again-and that is
well; but also she will never sit down on a cold one anymore.”
―Mark Twain \(Samuel Langhorne Clemens, 1835-1910\)"

   "“As in certain cults it is possible to kill a process
if you know its true name.”
―Ken Thompson and Dennis M. Ritchie"

   "“On two occasions I have been asked \[by members of Parliament!\], ‘Pray,
Mr.Babbage, if you put into the machine wrong figures, will the right
answers come out?’  I am not able rightly to apprehend the kind of
confusion of ideas that could provoke such a question.”
―Charles Babbage"

   "“Save the environment.  Create a closure today.”
―Cormac Flanagan"

   "“Price Wang’s programmer was coding software.  His fingers danced upon
the keyboard.  The program compiled without an error message, and the program
ran like a gentle wind.
‘Excellent!’ the Price exclaimed, ‘Your technique is faultless!’
‘Technique?’ said the programmer, turning from his terminal, ‘What I
follow is the Tao -- beyond all technique.  When I first began to program I
would see before me the whole program in one mass.  After three years I no
longer saw this mass.  Instead, I used subroutines.  But now I see nothing.
My whole being exists in a formless void.  My senses are idle.  My spirit,
free to work without a plan, follows its own instinct.  In short, my program
writes itself.  True, sometimes there are difficult problems.  I see them
coming, I slow down, I watch silently.  Then I change a single line of code
and the difficulties vanish like puffs of idle smoke.  I then compile the
program.  I sit still and let the joy of the work fill my being.  I close my
eyes for a moment and then log off.’
Price Wang said, ‘Would that all of my programmers were as wise!’”
―Geoffrey James, \"The Tao of Programming\""

   "“The Art of the Metaobject Protocol is the best book written in
computing in ten years. Java and C++ make you think that the new ideas
are like the old ones. Java is the most distressing thing to hit
computing since MS-DOS.”
―Alan Kay \(at OOPSLA97 - http://www.cc.gatech.edu/fac/mark.guzdial/squeak/oopsla.html\)"

   "“The best swordsman in the world doesn’t need to fear the second best
swordsman in the world; no, the person for him to be afraid of is some
ignorant antagonist who has never had a sword in his hand before; he
doesn’t do the thing he ought to do, and so the expert isn’t prepared
for him; he does the thing he ought not to do and often it catches the
expert out and ends him on the spot.”
―Mark Twain"

   "“The best way to have a good idea is to have a lot of
ideas and throw the bad ones away.”
―Linus Pauling"

   "“Двух вещей очень трудно избежать: тупоумия - если замкнуться
в своей специальности, и неосновательности - если выйти из нее.”
―В. Гёте"

   "“The Great Man ... is colder, harder, less hesitating,
and without respect and without the fear of \"opinion\";
he lacks the virtues that accompany respect and \"respectability\",
and altogether everything that is the \"virtue of the herd\".
If he cannot lead, he goes alone. ... He knows he is incommunicable:
he finds it tasteless to be familiar. ... When not speaking to himself,
he wears a mask. There is a solitude within him that is inaccessible
to praise or blame.”
―Friedrich Nietzche, The Will to Power"

   "“Ars longa, vita brevis, occasio praeceps,
experimentum periculosum, iudicium difficile.
Life short, \[the\] craft long, opportunity fleeting,
experiment treacherous, judjment difficult.”
―Hyppocrates \(c. 400BC\)"

   "“Anyone who cannot cope with mathematics is not fully human.
At best he is a tolerable subhuman who has learned to wear shoes,
bathe, and not make messes in the house.”
―Lazarus Long"

   "“Avoid making irrevocable decisions while tired or hungry.
N.B.: Circumstances can force your hand, so think ahead!”
―Lazarus Long"

   "“If it can’t be expressed in figures, it is not science; it is opinion.”
―Lazarus Long"

   "“One man’s ‘magic’ is another man’s engineering. ‘Supernatural’ is a null word.”
―Lazarus Long"

   "“You can go wrong by being too skeptical as readily as by being too trusting.”
―Lazarus Long"

   "“The two highest achievements of the human mind are the twin concepts of
‘loyalty’ and ‘duty.’ Whenever these twin concepts fall into disrepute –
get out of there fast! You may possibly save yourself,
but it is too late to save that society. It is doomed.”
―Lazarus Long"

   "“When a place gets crowded enough to require ID’s,
social collapse is not far away. It is time to go elsewhere.
The best thing about space travel is that it made it possible to go elsewhere.”
―Lazarus Long"

   "“The difference between science and the fuzzy subjects is that science
requires reasoning, while those other subjects merely require scholarship.”
―Lazarus Long"

   "“It’s far too late, and things are far too bad, for pessimism.”
―Dee. Hock, Founder, Visa International"

   "“Work expands so as to fill the time available for its completion.”
―Parkinson’s law"

   "“Science is the father of knowledge, but opinion breeds ignorance.”
―Hippocrates 460-377 BC"

   "“Everything that can be invented has been invented.”
―Charles H. Duell, Commissioner, U.S. Office of Patents"

   "“I think there’s a world market for about 5 computers.”
―Thomas J. Watson, Chairman of the Board, IBM"

   "“It would appear that we have reached the limits of what it is
possible to achieve with computer technology, although one should be careful
with such statements, as they tend to sound pretty silly in 5 years.”
―John Von Neumann"

   "“Controlling complexity is the essence of computer programming.”
―Brian Kernigan"

   "“Complexity kills.  It sucks the life out of developers,
it makes products difficult to plan, build and test,
it introduces security challenges,
and it causes end-user and administrator frustration.”
―Ray Ozzie"

   "“There are two ways of constructing a software design.
One way is to make it so simple that there are obviously no deficiencies.
And the other way is to make it so complicated that there are no obvious
deficiencies.”
―C.A.R. Hoare"

   "“True innovation often comes from the small startup who is
lean enough to launch a market but lacks the heft to own it.”
―Timm Martin"

   "“Nearly everything is really interesting if you go into it deeply enough.”
―Richard Feynman"

   "“The moment you think you know everything is the moment you stop learning.”
―Yeontabal, JuMong"

   "“Furious activity is no substitute for understanding.”
―H.H. Williams"

   "“You do not really understand something unless you can explain it to your
grandmother.”
―Albert Einstein."

   "“A journey of a thousand miles begins with a single step.”
―Lao Tzu"

   "“One never knows what one is going to do. One starts a painting and then
it becomes something quite different.”
―Pablo Picasso"

   "“Don’t worry about what anybody else is going to do. The best way to predict
the future is to invent it.”
―Alan Kay"

   "“Keep away from people who try to belittle your ambitions.
Small people always do that, but the really great make you feel that you, too,
can become great.”
―Mark Twain"

   "“However little television you watch, watch less.”
―David McCullough"

   "“Learning is not compulsory. Neither is survival.”
―W. Edwards Deming"

   "“Good judgement comes from experience, and experience comes from bad judgement.”
―Fred Brooks"

   "“One person with passion is better than forty people merely interested.”
―E.M. Forster"

   "“Just because it isn’t done doesn’t mean it can’t be done.
Just because it can be done doesn’t mean it should be.”
―Barry Glasford"

   "“Learn the rules so you know how to break them properly.”
―Dali Lama"

   "“Never attribute to malice what incompetence will explain.”
―Robert J. Hanlon"

   "Never assume malice when stupidity will suffice.
Never assume stupidity when ignorance will suffice.
Never assume ignorance when forgivable error will suffice.
Never assume error when information you hadn't adequately accounted for will suffice."

   "“Supposing is good, but finding out is better.”
―Samuel Clemens (Mark Twain)"

   "“Planning is essential, but plans are useless”
―Dwight D. Eisenhower"

   "“Ron’s first law: All extreme positions are wrong.”
―Ron Garret"

   "“We laugh at that which we cannot bear to face.”
―Aristotle"

   "“It is difficult to get a man to understand something when
his salary depends on his not understanding it.”
―Upton Sinclair"

   "“If you want to pass, cheat. If you want to learn, research.”
―Raganwald"

   "“I don’t know where I’m going, but I’m on my way.”
―Carl Sagan"

   "“What I cannot create, I do not understand.”
―Richard P. Feynman"

   "“Know how to solve every problem that has been solved.”
―Richard P. Feynman"

   "“The intellectual level needed for system design is in general grossly
underestimated. I am convinced more than ever that this type of work is very
difficult and that every effort to do it with other than the best people is
doomed to either failure or moderate success at enormous expense.”
―Edsger Dijkstra"

   "“Don’t tell me how hard you work. Tell me how much you get done.”
―James J. Ling"

   "“There is no reason to assume that the universe has the slightest interest
in intelligence—or even in both. Life may be random accidental by-products of
its operations like the beautiful patterns on a butterfly’s wings.
The insect would fly just as well without them.”
―Arthur C. Clarke, The Lost Worlds of 2001, 1972"

   "“If you want to achieve greatness, stop asking for permission.”
―Eddie Colla"

   "“Worry is like a rocking chair—it gives you something to do but doesn’t get you anywhere.”
―Erma Bombeck"

   "“Obsessed is a word the lazy use to describe the dedicated.”
―Russell Warren"

   "“In every passionate pursuit, the pursuit counts more than the object pursued.”
―Bruce Lee"

   "“When you’re screwing up, and nobody’s saying anything to you
anymore, that means they gave up.”
―Randy Pausch"

   "“Watch your thoughts; they become words. Watch your words; they become actions.
Watch your actions; they become habit. Watch your habits; they
become character. Watch your character; it becomes your destiny.”
―Lao Tzu (sixth century B.C.)"

   "“The only limits you have are the limits you believe.”
―Wayne Dyer"

   "“You cannot always control what goes on outside. But you can
always control what goes on inside.”
―Wayne Dyer"

   "“If you love a flower, don’t pick it up. Because if you pick
it up, it dies and ceases to be what you love. So if you love a
flower, let it be. Love is not about possession. Love is about
appreciation.”
―Osho"

   "“Forgive others not because they deserve forgiveness, but because you
deserve peace.”
―Jonathan Lockwood Huie"

   "“Humility is not thinking less of yourself, but thinking of yourself less.”
―C.S. Lewis"

   "“There is no thrill like that felt by the inventor as he sees his creation
unfolding to success. Such emotions make a man forget food, sleep, friends, love,
everything.”
―Nikola Tesla"

   "“Don’t rely on someone else for your self-worth; only you can
be responsible for that.”
―Stacey Charter"

   "“The first step towards getting somewhere is to decide that you are not going
to stay where you are.”
―J.P. Morgan"

   "“Life is like riding a bicycle. To keep your balance, you must keep moving.”
―Albert Einstein"

   "“You can never cross the ocean, unless you have the courage
to lose sight of the shore.”
―Columbus"

   "“Pain is inevitable. Suffering is optional.”
―M. Kathleen Casey"

   "“We must be willing to let go of the life we’ve planned, so as to have the
life that is waiting for us.”
―Joseph Campbell"

   "“The best way out is always through.”
―Robert Frost"

   "“The difference between ordinary and extraordinary is that little extra.”
―Jimmy Johnson"

   "“What we do for ourselves dies with us. What we do for others remains forever.”
―Albert Pike"

   "“Do what you can, where you are, with what you have.”
―Theodore Roosevelt"

   "“What we fear doing most is usually what we most need to do.”
―Tim Ferriss"

   "“If you aren’t going all the way, why go at all.”
―Joe Namath"

   "“You miss 100% of the shots you don’t take.”
―Wayne Gretzky"

   "“It’s not whether you get knocked down, it’s whether you get up.”
―Vince Lombardi"

   "“It’s better to be alone rather than being with someone who makes you feel
like you’re alone.”
―Ivan Junius"

   "“If you do what you’ve always done, you’ll get what you’ve always gotten.”
―Anthony Robbins"

   "“If you want something you’ve never had, you must be willing
to do something you’ve never done.”
―Thomas Jefferson"

   "“The best way to find out if you can trust somebody is to trust them.”
―Ernest Hemingway"

   "“You can’t have a better tomorrow, if you’re still thinking about yesterday.”
―Charles Kettering"

   "“Catch a fish once and if it gets away, it’s a lot harder to catch the second time.”
―Will Graham"

   "“A life without regret would be no life at all.”
―Hannibal Lecter"

   "“Nothing makes us more vulnerable than loneliness, except greed.”
―Bedelia Du Maurier"

   "“Those who would give up essential liberty, to purchase a little temporary
safety, deserve neither liberty nor safety.”
―Benjamin Franklin"

   "“Go to the edge of the cliff and jump off. Build your wings on the way down.”
―Ray Bradbury"

   "“A common man marvels at uncommon things; a wise man marvels at the commonplace.”
―Confucius"

   "“Admiration is happy self-surrender; envy is unhappy self-assertion.”
―Søren Kierkegaard"

   "“The greatest danger occurs at the moment of victory.”
―Napoleon Bonaparte"

   "“Upon this, one has to remark that men ought either to be
well treated or crushed, because they can avenge themselves of
lighter injuries, of more serious ones they cannot; therefore the
injury that is to be done to a man ought to be of such a kind
that one does not stand in fear of revenge.”
―Niccolò Machiavelli"

   "“The laws that govern circumstances are abolished by new circumstances.”
―Napoleon Bonaparte"

   "“A military force has no constant formation, water has no constant shape:
The ability to gain victory by changing and adapting according to the opponent is
called genius.”
―Sun-tzu"

   "“… the man who says it can’t be done is generally interrupted
by someone doing it.”
―Elbert Hubbard"

   "“The Universe is under no obligation to make sense to you.”
―Neil deGrasse Tyson"

   "“You are the average of the five people you spend the most time with.”
―Jim Rohn"

   "“Some things are up to us, and some things are not up to us.”
―Epictetus, Stoic philosopher"

   "“I returned, and saw under the sun, that the race is not to the
swift, nor the battle to the strong, neither yet bread to the
wise, nor yet riches to men of understanding, nor yet favour to
men of skill; but time and chance happeneth to them all.”
—קֹהֶלֶת (translation: Ecclesiastes 9:11, King James' Bible)"

   "“You can, for example, never foretell what any one man will
do, but you can say with precision what an average number will be
up to. Individuals vary, but percentages remain constant. So says
the statistician.”
— Sherlock Holmes / Sir Arthur Conan Doyle, The Sign of Four"

   "“The job of a financial mathematician is not to predict prices
but instead to relate the movements of price in one asset to that
of another.”
— page 4, Mark Joshi, The Concepts and Practice of Mathematical Finance, Cambridge University Press, 2003"

   "“Mathematical bankers generally search for the arbitrage
opportunities under some assumptions. Whilst all of these
assumptions can be criticised, they provide a good starting point
for modelling. The objective is more to come up with a good model
than a perfect description. There is a certain similarity to
physics here. Newtonian physics makes certain assumptions about
the nature of space and time which are demonstrably wrong.
However, bridges are built with Newtonian physics and they do not
fall down (or at least not very often). The reason is that
Newtonian physics provides a good approximation in the everyday
world which only breaks down in the small subatomic world and the
huge astronomical scale. Similarly, the models of mathematical
finance provide good approximations under what one might call
‘normal’ conditions, but they may perform less well in
extremeties. However, just as in physics, the fact that models
are not universally valid actually keeps people in work.”
— page 19, Mark Joshi, The Concepts and Practice of Mathematical Finance, Cambridge University Press, 2003"

    "“There is a tide in the affairs of men,
Which, taken at the flood, leads on to fortune;
Omitted, all the voyage of their life
Is bound in shallows and miseries.
We must take the current when it serves,
Or lose our ventures.”
— Shakespeare"

    "“I think that it’s extraordinarily important that we in computer
science keep fun in computing. When it started out, it was an awful
lot of fun. Of course, the paying customers got shafted every now and
then, and after a while we began to take their complaints seriously.
We began to feel as if we really were responsible for the successful,
error-free perfect use of these machines. I don’t think we are. I
think we’re responsible for stretching them, setting them off in new
directions, and keeping fun in the house. I hope the field of computer
science never loses its sense of fun. Above all, I hope we don’t
become missionaries. Don’t feel as if you’re Bible salesmen. The world
has too many of those already. What you know about computing other
people will learn. Don’t feel as if the key to successful computing is
only in your hands. What’s in your hands, I think and hope, is
intelligence: the ability to see the machine as more than when you
were first led up to it, that you can make it more.”
―Alan Perlis, The Structure and Interpretation of Computer Programs"

    "“If you want more effective programmers, you will discover that
they should not waste their time debugging, they should not introduce
the bugs to start with.”
—Edsger Dijkstra, Turing Award Lecture (1972)"

     "No matter how much money you spend, you can't make a racehorse out of a
pig.  You can, however, make an awfully fast pig.
– An old saying about program efficiency"

    "Histocompatibility, Disease and Aging
	poem on a paper by E. Yunis

“The crown of life, our play's last act,”
Cicero on old age was opining.
What he didn’t know, but now is a fact:
It’s then your T-cells are declining,
Too many tick-tocks of the old thymic clock;
It runs down like a watch on the shelf.
Then suppressor T-cells aren’t sufficient to block
B-cell clones that arise against self.
This theory’s supported, Dr. Yunis explained,
By studies in mice and in man.
The data suggest that the program’s ingrained;
It’s a genetic kind of a plan.
It seems to depend on your HLA type.
If you have a desire to die late,
And your wish is, in time, to become overripe,
It is better not to B-8.
–Donald Patterson

reprinted from 9 April 1982 issue of “Science”
reprinted from “Institute of Laboratory Animal Resources News”,
25 (No. 2), 6 (winter 1982)"

    "Increasingly, people seem to misinterpret complexity as
sophistication, which is baffling – the incomprehensible should cause
suspicion rather than admiration. Possibly this trend results from a
mistaken belief that using a somewhat mysterious device confers an
aura of power on the user.
–Niklaus Wirth"

;; Mail should be at least a mixture of upper and lower case.  Devising
;; your own font (Devanagari, pinhead graphics, etc.) and using it in the
;; mail is a good entertainment tactic, as is finding some way to use
;; existing obscure fonts.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; It is considered artful to append many messages on a subject, leaving
;; only the most inflammatory lines from each, and reply to all in one
;; swift blow.  The choice of lines to support your argument can make or
;; break your case.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Replying to one's own message is a rarely-exposed technique for
;; switching positions once you have thought about something only after
;; sending mail.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; State opinions in the syntax of fact: "...as well as the bug in LMFS
;; where you have to expunge directories to get rid of files....."
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; If you have nothing to say on a subject, replying with a line such as,
;; "I agree with this." puts you in the TO:'s for all future messages, and
;; establishes you as "one who really cares", if not an actual expert, on
;; the topic at hand.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Inclusion of very old messages from others makes for an impressive show.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; People can be set wondering by loading obscure personal patchable
;; systems, and sending bug reports.  Who would not stop and wonder upon
;; seeing "Experimental TD80-TAPE 1.17, MegaDeath 2.5..."?  The same for
;; provocatively-named functions and variables in stack traces.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Know the list of "large, chronic problems".  If there is any problem
;; with the window system, blame it on the activity system.  Any lack of
;; user functionality should be attributed to the lack of a command
;; processor.  A suprisingly large number of people will believe that you
;; have thought in depth about the issue to which you are alluding when you
;; do.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Know how to blow any problem up into insolubility.  Know how to use the
;; phrase "The new ~A system" to insult its argument, e.g., "I guess this
;; destructuring LET thing is fixed in the new Lisp system", or better yet,
;; PROLOG.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Never hit someone head on, always sideswipe.  Never say, "Foo's last
;; patch was brain-damaged", but rather, "While fixing the miscellaneous
;; bugs in 243.xyz [foo's patch], I found...."
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Idiosyncratic indentations, double-spacing, capitalization, etc., while
;; stamps of individuality, leave one an easy target for parody.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Strong language gets results.  "The reloader is completely broken in
;; 242" will open a lot more eyes than "The reloader doesn't load files
;; with intermixed spaces, asterisks, and <'s in their names that are
;; bigger than 64K".  You can always say the latter in a later paragraph.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Including a destination in the CC list that will cause the recipients'
;; mailer to blow out is a good way to stifle dissent.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; When replying, it is often possible to cleverly edit the original
;; message in such a way as to subtly alter its meaning or tone to your
;; advantage while appearing that you are taking pains to preserve the
;; author's intent.  As a bonus, it will seem that your superior
;; intellect is cutting through all the excess verbiage to the very heart
;; of the matter.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Referring to undocumented private communications allows one to claim
;; virtually anything: "we discussed this idea in our working group last
;; year, and concluded that it was totally brain-damaged".
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Points are awarded for getting the last word in.  Drawing the
;; conversation out so long that the original message disappears due to
;; being indented off the right hand edge of the screen is one way to do
;; this.  Another is to imply that anyone replying further is a hopeless
;; cretin and is wasting everyone's valuable time.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Keeping a secret "Hall Of Flame" file of people's mail indiscretions,
;; or copying messages to private mailing lists for subsequent derision,
;; is good fun and also a worthwhile investment in case you need to
;; blackmail the senders later.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Users should cultivate an ability to make the simplest molehill into a
;; mountain by finding controversial interpretations of innocuous
;; sounding statements that the sender never intended or imagined.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Obversely, a lot of verbal mileage can also be gotten by sending out
;; incomprehensible, cryptic, confusing or unintelligible messages, and
;; then iteratively "correcting" the "mistaken interpretations" in the
;; replys.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Trivialize a user's bug report by pointing out that it was fixed
;; independently long ago in a system that hasn't been released yet.
;;                 -- from the Symbolics Guidelines for Sending Mail
;; %
;; Send messages calling for fonts not available to the
;; recipient(s).  This can (in the case of Zmail) totally disable
;; the user's machine and mail system for up to a whole day in some
;; circumstances.
;;                 -- from the Symbolics Guidelines for Sending Mail
]
  "Good and nice fortunes.")

(defvar *fortunes*
  *good-fortunes*)

(defun fortune--reschedule-queue ()
  "Return queue that with all indices of fortunes
in ‘*fortunes*’ shuffled in random order.

Queue is just a list actually."
  (let ((vect (make-vector (length *fortunes*) 0)))
    (loop
      for i below (length vect)
      do (setf (aref vect i) i))
    (random-shuffle vect *random-gen*)
    (loop
      for i across vect
      collect (aref vect i))))

(defun fortune--merge-fortune-queues (old new)
  ;; Pick the longest list since it’s the oldest one we’re trying to preserve,
  ;; most of the time.
  (if (< (length old) (length new))
      new
    old))

;;;###autoload
(add-to-list 'persistent-store-merge-handlers
             (cons 'fortunes-fortune-queue #'fortune--merge-fortune-queues))

;;;###autoload
(defun fortunes-get-next-fortune ()
  "Return next queued fortune using persistent queue and
make up new queue if persistent one is empty."
  (let ((fortune-queue (persistent-store-get 'fortunes-fortune-queue)))
    (while (and (not (null (car fortune-queue)))
                (> (car fortune-queue)
                   (length *fortunes*)))
      (pop fortune-queue))
    (unless fortune-queue
      (setq fortune-queue (fortune--reschedule-queue)))
    (prog1 (aref *fortunes*
                 (min (car fortune-queue) (1- (length *fortunes*))))
      (persistent-store-put 'fortunes-fortune-queue (cdr fortune-queue)))))

;;;###autoload
(defun fortunes-init-scratch-buffer ()
  "Put fortune into scratch buffer."
  (random t)
  (setf initial-scratch-message
        (fortunes-comment-out-fortune (fortunes-get-next-fortune)))
  (awhen (get-buffer "*scratch*")
    (set-buffer-major-mode it)))

;;;###autoload
(defun fortunes-comment-out-fortune (fortune-text)
  (mapconcat (lambda (x) (concat ";; " x))
             (split-into-lines fortune-text)
             "\n"))

(provide 'fortunes)

;; Local Variables:
;; End:

;; fortunes.el ends here
