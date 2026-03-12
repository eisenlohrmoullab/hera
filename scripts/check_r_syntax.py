#!/usr/bin/env python3
"""
Basic R syntax checker for run_smm_cyclic.R
Checks for common syntax errors without requiring R installation

NOTE: This checker has limitations:
- Counts delimiters globally without parsing strings or comments
- May produce false positives for unbalanced delimiters in string literals
- Does not perform full R syntax validation
"""

import sys

def check_r_syntax(filepath):
    """Check R file for basic syntax issues"""
    errors = []
    warnings = []
    
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Check for balanced braces
    open_braces = content.count('{')
    close_braces = content.count('}')
    if open_braces != close_braces:
        errors.append(f"Unbalanced braces: {open_braces} opening vs {close_braces} closing")
    
    # Check for balanced parentheses
    open_parens = content.count('(')
    close_parens = content.count(')')
    if open_parens != close_parens:
        errors.append(f"Unbalanced parentheses: {open_parens} opening vs {close_parens} closing")
    
    # Check for balanced square brackets
    open_brackets = content.count('[')
    close_brackets = content.count(']')
    if open_brackets != close_brackets:
        errors.append(f"Unbalanced brackets: {open_brackets} opening vs {close_brackets} closing")
    
    # Check for common function names being used
    required_funcs = ['glue', 'plogis', 'binomial', 'gaussian', 'gamm4', 'gam']
    for func in required_funcs:
        if func in content:
            print(f"✓ Function '{func}' is used")
    
    # Check for key binary outcome handling code
    checks = [
        ('is_binary <- all(data', 'Binary detection logic'),
        ('family_spec <- binomial', 'Binomial family specification'),
        ('family_spec <- gaussian', 'Gaussian family specification'),
        ('plogis(preds)', 'Inverse logit transformation'),
        ('fitted_probs * (1 - fitted_probs)', 'Binomial variance calculation'),
        ('y_label_roll', 'Dynamic y-axis label'),
        ('y_limits', 'Dynamic y-axis limits'),
    ]
    
    for code_snippet, description in checks:
        if code_snippet in content:
            print(f"✓ {description} present")
        else:
            warnings.append(f"Missing {description}")
    
    # Report results
    print("\n" + "="*60)
    if errors:
        print("ERRORS FOUND:")
        for error in errors:
            print(f"  ✗ {error}")
    else:
        print("✓ No syntax errors detected")
    
    if warnings:
        print("\nWARNINGS:")
        for warning in warnings:
            print(f"  ⚠ {warning}")
    
    print("="*60)
    
    return len(errors) == 0

if __name__ == '__main__':
    filepath = 'scripts/run_smm_cyclic.R'
    success = check_r_syntax(filepath)
    sys.exit(0 if success else 1)
