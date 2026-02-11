# Program Analysis Bootcamp

> Learn to build automated tools for understanding, analyzing, and improving code quality.

## Course Overview

This intensive bootcamp teaches practical program analysis techniques used in modern software development. Students learn to build static analyzers, AST-based tools, and dataflow analysis engines while understanding the theoretical foundations.

**Duration:** 3 weeks (Modules 1-3)
**Format:** Theory + Hands-on Labs
**Prerequisites:** Basic programming experience (Python and/or JavaScript)

## Quick Start

### Prerequisites
- Git and GitHub account
- Python 3.8+
- Node.js 16+ (Module 1 only)
- Code editor (VS Code recommended)

### Setup
```bash
# Clone the repository
git clone https://github.com/your-org/program-analysis-bootcamp.git
cd program-analysis-bootcamp

# Run setup script
./scripts/setup-environment.sh
```

### Course Structure
```
Week 1: Foundations of Program Analysis
Week 2: Code Representation & Abstract Syntax Trees
Week 3: Static Analysis Fundamentals
```

## Modules

| Module | Topic | Duration | Key Deliverable |
|--------|-------|----------|-----------------|
| [1](modules/module1-foundations/) | Foundations of Program Analysis | 6h | Analysis comparison report |
| [2](modules/module2-ast/) | Code Representation & ASTs | 6h | AST parser & transformations |
| [3](modules/module3-static-analysis/) | Static Analysis Fundamentals | 6h | Dataflow analysis engine |

## Labs

| Lab | Topic | Duration | Points |
|-----|-------|----------|--------|
| [1](labs/lab1-tool-setup/) | Tool Setup & Verification | 30 min | -- |
| [2](labs/lab2-ast-parser/) | AST Parser & Analyzer | 3h | 100 |
| [3](labs/lab3-static-checker/) | Static Analysis Checker | 4h | 100 |

## Mini-Projects

| Week | Project | Description |
|------|---------|-------------|
| [1](projects/mini-projects/week1-linter/) | Custom Linter | Build a Python linter with 5+ rules |
| [2](projects/mini-projects/week2-ast-visualizer/) | AST Visualizer | Render AST tree diagrams |
| [3](projects/mini-projects/week3-bug-finder/) | Bug Finder | CFG + dataflow bug detector |

## Tools & Technologies

**Analysis Tools:**
- ESLint (static analysis, Module 1)
- Python `ast` module (AST manipulation, Modules 2-3)
- Custom dataflow framework (Module 3)

**Development Environment:**
- Python 3.8+ for Modules 2-3
- JavaScript/Node.js for Module 1
- pytest for testing

## Assessment

| Component | Weight | Description |
|-----------|--------|-------------|
| Quizzes | 18% | 3 module quizzes (6% each) |
| Labs | 24% | 2 graded labs (12% each) |
| Mini-Projects | 30% | 3 progressive projects (10% each) |
| Participation | 28% | Exercises and engagement |

## Learning Outcomes

By the end of this bootcamp, you will be able to:
- Evaluate trade-offs between static and dynamic analysis approaches
- Build and traverse Abstract Syntax Trees programmatically
- Implement dataflow analysis frameworks (reaching definitions, live variables)
- Construct control flow graphs from source code
- Design simple static analysis tools

## Resources

- [Course Syllabus](SYLLABUS.md)
- [Setup Guides](resources/tools/installation-guides/)
- [Sample Programs](resources/datasets/sample-programs/)

## Support

- **Issues:** Use GitHub Issues for technical problems
- **Discussions:** Course Slack/Discord channel
- **Office Hours:** See syllabus for schedule
