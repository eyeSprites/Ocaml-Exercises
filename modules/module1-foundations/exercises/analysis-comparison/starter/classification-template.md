# Analysis Classification Exercise

## Instructions
For each code snippet in `code-samples.md`, fill in the table below.

**Objective categories:** Correctness, Security, Performance
**Detection method:** Static, Dynamic, Both

---

| Snippet | Issue Description | Objective | Detection Method | Explanation |
|---------|-------------------|-----------|-----------------|-------------|
| 1 | SQL injection vulnerability | Security | Static | String concatenation creates SQL injection risk |
| 2 | Unreachable code | Correctness | Static | Code after return statement never executes |
| 3 | Division by zero risk | Correctness | Both | Empty list causes len(numbers) = 0 |
| 4 | Buffer overflow | Security | Static | No bounds checking for destination buffer |
| 5 | Array index out of bounds | Correctness | Static | Loop condition uses <= instead of < |
| 6 | Inefficient recursion | Performance | Static | Exponential time complexity, redundant calculations |
| 7 | Resource leak | Correctness | Static | File handle never closed |
| 8 | Command injection | Security | Static | User input directly passed to system command |
| 9 | Memory leak | Performance | Dynamic | Cache grows indefinitely without cleanup |
| 10 | Dead code | Correctness | Static | Code after return statement unreachable |
| 11 | Transaction consistency | Correctness | Both | Exception between withdraw/deposit breaks atomicity |
| 12 | Inefficient algorithm | Performance | Static | Nested loop with redundant inner iteration |
| 13 | XSS vulnerability | Security | Static | Direct HTML injection without sanitization |
| 14 | Division by zero risk | Correctness | Both | Zero divisor causes runtime error |
| 15 | Dangling pointer | Correctness | Static | Returns pointer to local variable |

---

## Summary Questions

### How many snippets had Correctness issues? 8
### How many had Security issues? 4
### How many had Performance issues? 3

### Which issues are best caught by static analysis? Why?
Most issues can be caught statically, including SQL injection, buffer overflows, unreachable code, array bounds errors, resource leaks, and algorithm inefficiencies. Static analysis can detect these by examining code structure, data flow, and patterns without execution.

### Which issues require dynamic analysis? Why?
Memory leaks and runtime exceptions need dynamic analysis to observe actual behavior and resource usage over time.

