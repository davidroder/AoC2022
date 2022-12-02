>>>,[  (4)=inp
    >+[ (5) = 1
        [    
            [-]<<  (5)=0 ; to 3
            [>>+<<-] (5)=(3);(3)=0
            >> to 5
            [<<++++++++++>>-] (3)=10*(5)
            < to 4
            [<+>-]   (3)=(3)add(4); (4)=0; to 4
            >++++++[<++++++++>-]<  (4)=48
            [<->-] (3)= (3)sub(4); (4)=0; to 4
            >> to 6
            , (6)=inp
            [<+<+>>-] (4&5)=(6)
            ++++[<-------->-] (5)= (5)sub32; to 6
            < to 5
        ] loop if inp wasnt 32; to 5
        < [-] (4)=0; to 4
        < [<+>-] (2)=(2) add (3); to 3
        ,  (3)=inp; to 3
        [>+>+<<-]>>> (4&5)=inp; to 6
        ++++[<-------->-] (5)sub32 in (5); to 6
        < to 5
    ] loop if inp wasnt 32; to 5
    < [-] (4) = 0 ; to 4
    <<< ; to 1
    [>>+<<-] (3)=(1); (1)=0; to 1
    >>>>+ (5)=1
    <<<[ while (2); to 2
        <+ (1)=(1) plus 1; to 1
        >> to 3
        [->] if (3)!=0 then{ (3)=(3)sub1 ; to 4} else to 3
        > to 4 or 5
        [<] if (4|5)!=0 (ie at 5) then to 4
        << to 2
        - (2)=(2) sub 1)
    ] loop if (2)!= 0
    >>>- (5)=0
    << to 3
    [<<+>>-] (1) = (1)add(3)
    > to 4
,]
<<< to 1
[>+<-] (2)=(1) we need an empty space to mark the start
(when we print in reverse)
> to 2
[ locations in this loop relative (increasing each time)
    init 1 is outer 2
    [>+<-] (2)=(1); (1)=0
    >>> to 4
    + (4)=1
    << to 2
    [
        - <+> rem in (1)
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <[-]> >]>[>+<<]< div in (5)
    ]
    >>- (4)=0
    > to 5
    [<<<+>>>-] (2)=(5); (5)=0
    << to 3
    ++++++[<<++++++++>>-] (1)=(1)add48
    < to 2
]
<[.<] output chars in reverse




part 2
>>>>>,[  (4)=inp
    >+[ (5) = 1
        [    
            [-]<<  (5)=0 ; to 3
            [>>+<<-] (5)=(3);(3)=0
            >> to 5
            [<<++++++++++>>-] (3)=10*(5)
            < to 4
            [<+>-]   (3)=(3)add(4); (4)=0; to 4
            >++++++[<++++++++>-]<  (4)=48
            [<->-] (3)= (3)sub(4); (4)=0; to 4
            >> to 6
            , (6)=inp
            [<+<+>>-] (4&5)=(6)
            ++++[<-------->-] (5)= (5)sub32; to 6
            < to 5
        ] loop if inp wasnt 32; to 5
        < [-] (4)=0; to 4
        < [<+>-] (2)=(2) add (3); to 3
        ,  (3)=inp; to 3
        [>+>+<<-]>>> (4&5)=inp; to 6
        ++++[<-------->-] (5)sub32 in (5); to 6
        < to 5
    ] loop if inp wasnt 32; to 5
    < [-] (4) = 0 ; to 4
    <<< ; to 1
    [>>+<<-] (3)=(1); (1)=0; to 1
    >>>>+ (5)=1
    <<<[ while (2); to 2
        <+ (1)=(1) plus 1; to 1
        >> to 3
        [->] if (3)!=0 then{ (3)=(3)sub1 ; to 4} else to 3
        > to 4 or 5
        [<] if (4|5)!=0 (ie at 5) then to 4
        << to 2
        - (2)=(2) sub 1)
    ] loop if (2)!= 0
    >>>- (5)=0
    << to 3
    [<<+>>-] (1) = (1)add(3)
    > to 4
,]
<<< to 1
[>+<-] (2)=(1) we need an empty space to mark the start
(when we print in reverse)
> to 2
[ locations in this loop relative (increasing each time)
    init 1 is outer 2
    [>+<-] (2)=(1); (1)=0
    >>> to 4
    + (4)=1
    << to 2
    [
        - <+> rem in (1)
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <+> >]>[<]<
        [- <[-]> >]>[>+<<]< div in (5)
    ]
    >>- (4)=0
    > to 5
    [<<<+>>>-] (2)=(5); (5)=0
    << to 3
    ++++++[<<++++++++>>-] (1)=(1)add48
    < to 2
]
<[.<] output chars in reverse