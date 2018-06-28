# Multi-Agents-Vervets

Monk is a efficient functional implementation of multi-agents lexical convergence based in this project https://github.com/caotic123/Intelligent-Monkeys using the following paper https://pt.scribd.com/document/382793399/Msc-Angelo by Angelo Conrado - Universidade Estudal de Campinas(Campinas State University), however, the goal of the project is a  lexical learning(and patterns of this....)

# Abstraction

Vertets Monkeys are animals that can communicate throught of screams(alert), hear this(hearing) and by final memorizing for identify preys, this is only possible if the "agents"(monkeys) uses a type of comunication and interact with each one.

# Practical
```
- - - - - - - - -
- - M1 - - M2 - P
- - - P - - - -
- - - - - - - -
M3 - - - - - - -
```
```
M1 = Lex Snake "AB" "AC"
M2 = Lex Snake "DB" "DK"
M3 = Lex Snake "SD" "AS"
```

Given a matrix (8x8) where M = Monkey and P = Prey(Maybe Tiger, Eagle or Snake) in this case let's consider P = Snake
M2 is the area of vision of P(snake), then M2 will alert the M1 of P.
```
M2 -> Alert M1 of P1
M2 -> M2 send a message "DB" that M2 "guess/think" will interpreter
(The lexical most equal of M1 to M2 is "DB" with "AB", so M2 will "DB" like alert for Snake)
M1 -> M1 will interpreted the message sended by M2
(M1 will interpreter the message as a msg more equal with your lexical "DB" is more equal with "AB")
M1 -> M1 will learning the new word equaling the words.
(The word "AB" will be a new word "BD")
```
# Results

In the begin of the project i thought the convergence will generates just one word able to comunicate, but this is not truth the system is able for generates many patterns of lexical for just one agent.
In my tests in 10.000 loops one words is the certainlyy the most equal with lexical of others monkeys but exist (more or less) 3 or 5 that almost are equal and this mean that the system is able for generates many combinations of lexical scheme.

