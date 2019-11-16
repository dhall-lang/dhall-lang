# Sphinx project configuration
project = 'Dhall'
copyright = '2019, Dhall Contributors'
author = 'Dhall Contributors'
master_doc = 'index'

pygments_style = 'sphinx'
html_theme_options = {
    'show_related': True,
    'page_width': 'auto',
    'github_user': 'dhall-lang',
    'github_repo': 'dhall-lang',
    'logo': 'dhall-logo.svg',
    'fixed_sidebar': True,
}
html_static_path = ['_static']

# Add markdown support
from recommonmark.parser import CommonMarkParser
source_suffix = ['.md']
source_parsers = {
    '.md': CommonMarkParser,
}

# Install AutoStructify to support toctree through eval_rst
from recommonmark.transform import AutoStructify

def setup(app):
    app.add_config_value('recommonmark_config', {
        'enable_auto_toc_tree': False,
    }, True)
    app.add_transform(AutoStructify)

# Add a custom lexer for dhall pygments
from pygments.lexer import RegexLexer
from pygments import token
from sphinx.highlighting import lexers

# TODO: finish the lexer
class DhallLexer(RegexLexer):
    name = 'dhall'

    tokens = {
        'root': [
            (r'(let|in|merge)', token.Keyword),
            (r'(=|->)', token.Operator),
            (r'-- .*?$', token.Comment),
            (r'.', token.Text),
        ]
    }
lexers['dhall'] = DhallLexer(startinline=True)

# Alias for compat with github syntax highligher name
from pygments.lexers.shell import BashLexer
from pygments.lexers.haskell import HaskellLexer
lexers['bash'] = BashLexer(startinline=True)
lexers['haskell'] = HaskellLexer(startinline=True)
