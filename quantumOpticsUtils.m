truncatedAnnihilationOp[truncation_Integer] := SparseArray[
    {
        {i_, j_} /; (i == j - 1 && j >= 1) :> Sqrt@i
    },
    {truncation, truncation}
]

truncatedCreationOp[truncation_Integer] := SparseArray[
    {
        {i_, j_} /; (i == j + 1 && i >= 1) :> Sqrt@j
    },
    {truncation, truncation}
]

truncatedNumberOp[truncation_Integer] := SparseArray[
    {
        {i_, j_} /; (i == j && i <= truncation) :> i
    },
    {truncation, truncation}
]

truncatedDisplacementOp[\[Alpha]_, truncation_Integer] := Times[
    Exp[-Abs[\[Alpha]]^2/2],
    Dot[
        MatrixExp[\[Alpha] truncatedCreationOp@truncation],
        MatrixExp[-Conjugate@\[Alpha] truncatedAnnihilationOp@truncation]
    ]
]