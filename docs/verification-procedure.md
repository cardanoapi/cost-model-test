# Verification of `minFeeRefScriptCostPerByte`

### Procedure

1. **Smart Contract 1 (C1)**
    - Deploy C1.
    - Deploy C1 again, adding a `traceIfFalse` function with a message `msg`.

2. **Smart Contract 2 (C2)**
    - Deploy C2.
    - Deploy C2 again, adding a `traceIfFalse` function with the same message `msg`.

### Notations

- Let:
  - Contract without `traceIfFalse` be denoted as C.
  - Contract with `traceIfFalse` be denoted as C<sup>T</sup>.
  - Fee function be denoted as F().
  - Bytes function be denoted as B().

### Calculations

1. **Difference in Fees and Script Sizes**
    - The difference in fees between the traced and untraced versions of a contract:
      - Δ Fee<sub>C</sub> = F(C<sup>T</sup>) - F(C)
    - The difference in script sizes (in bytes) between the traced and untraced versions of a contract:
      - Δ Bytes<sub>C</sub> = B(C<sup>T</sup>) - B(C)

2. **Reference Script Testing**
    - Use all four contracts (C1, C1<sup>T</sup>, C2, C2<sup>T</sup>) as reference scripts to redeem a UTxO.
    - Record the bytes and fees for each case.
    - Calculate the differences in script size and fees between traced and untraced versions for both C1 and C2:
      - For C1:
        - Δ Bytes<sub>C1</sub> = B(C1<sup>T</sup>) - B(C1)
        - Δ Fee<sub>C1</sub> = F(C1<sup>T</sup>) - F(C1)
      - For C2:
        - Δ Bytes<sub>C2</sub> = B(C2<sup>T</sup>) - B(C2)
        - Δ Fee<sub>C2</sub> = F(C2<sup>T</sup>) - F(C2)

3. **Expected Cost Calculation**
    - Since the reference script size is less than 25,000 bytes, the increment in cost should be linear (15 lovelace per byte).
    - Calculate the expected cost due to the difference in reference script sizes:
      - For C1:
        - F(Δ Bytes<sub>C1</sub>) = 15 * Δ Bytes<sub>C1</sub>
      - For C2:
        - F(Δ Bytes<sub>C2</sub>) = 15 * Δ Bytes<sub>C2</sub>

### Conclusion

- The difference between the actual fee difference and the expected cost due to script size should be consistent across both contracts:
  - Δ Fee<sub>C1</sub> - F(Δ Bytes<sub>C1</sub>) == Δ Fee<sub>C2</sub> - F(Δ Bytes<sub>C2</sub>)
- This consistency indicates that the `minFeeRefScriptCostPerByte` is accurately calculated, as the cost associated with the `traceIfFalse` function remains constant across different contracts.

### Verification Requirements

- Ensure both contracts have the same number of boolean validators and `&&` operations.
- Ensure the `traceIfFalse` function uses the same message in both contracts.
- make sure that both contracts use same datum and redeemer

