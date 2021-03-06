%%
%% This is file `cnx.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% cnx.dtx  (with options: `cls')
%% 
\RequirePackage{sref}
\RequirePackage{graphicx}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\ExecuteOptions{letterpaper}
\ProcessOptions
\input{size10.clo}
\pagenumbering{roman}
\addmetakey{cnxmodule}{name}
\srefaddidkey{cnxmodule}{id}
\newenvironment{cnxmodule}[1][]{\metasetkeys{cnxmodule}{#1}}{}
\newenvironment{ccontent}{}{}
\newcounter{section}
\srefaddidkey{sectioning}{id}
\newenvironment{csection}[2][]%
{\stepcounter{section}\strut\\[1.5ex]\noindent%
{\Large\bfseries\arabic{section}.~{#2}}\\[1.5ex]
\metasetkeys{sectioning}{#1}}
{}
\newcounter{subsection}[section]
\newenvironment{csubsection}[2][]
{\refstepcounter{subsection}\strut\\[1ex]\noindent%
{\large\bfseries{\arabic{section}.\arabic{subsection}.~#2\\[1ex]}}%
\metasetkeys{sectioning}{#1}}%
{}
\newcounter{subsubsection}[subsection]
\newenvironment{csubsubsection}[2][]
{\refstepcounter{subsubsection}\strut\\[.5ex]\noindent
{\bfseries\arabic{section}.\arabic{subsection}.\arabic{subsubsecction}~#2\\[.5ex]}%
\metasetkeys{sectioning}{#1}}{}
\srefaddidkey{para}{id}
\newenvironment{cpara}[1][]{\metasetkeys{para}{#1}}{\par}
\srefaddidkey{note}
\addmetakey{note}{type}
\newenvironment{cnote}[1][]%
{\metasetkeys{note}{#1}\par\noindent\strut\hfill\begin{minipage}{10cm}{\bfseries\note@type}:~}%
{\end{minipage}\hfill\strut\par}
\srefaddidkey{equation}{id}
\newenvironment{cequation}[1][]%
{\metasetkeys{equation}{#1}\begin{displaymath}}
{\end{displaymath}}
\srefaddidkey{cquote}
\addmetakey{cquote}{type}
\addmetakey{cquote}{src}
\newenvironment{cquote}[1][]{%
\metasetkeys{cquote}{#1}\begin{center}\begin{minipage}{.8\textwidth}}{\end{minipage}\end{center}}
\newcommand\litem[2][]{\item[#1]\label{#2}}
\srefaddidkey{example}
\addmetakey{example}{name}
\newenvironment{cexample}[1][]{\metasetkeys{example}{#1}
{\ifx\example@name\@empty\else\noindent\bfseries{\example@name}\fi}}
{}
\newcounter{cexercise}
\srefaddidkey{cexercise}
\addmetakey{cexercise}{name}
\newenvironment{cexercise}[1][]{\metasetkeys{cexercise}{#1}
{\ifx\cexercise@name\@empty\else\stepcounter{cexercise}\noindent\bfseries{\cexercise@name~\arabic{cexercise}}\fi}}
{}
\srefaddidkey{cproblem}
\newenvironment{cproblem}[1][]{\metasetkeys{cproblem}{#1}}{}{}
\srefaddidkey{csolution}
\newenvironment{csolution}[1][]{\metasetkeys{csolution}{#1}}{\par\noindent\bfseries{Solution}}{}
\srefaddidkey{rule}
\addmetakey{rule}{name}
\addmetakey{rule}{type}
\newenvironment{crule}[1][]{\metasetkeys{rule}{#1}%
{\noindent\bfseries{\rule@type:}\ifx\rule@name\@empty\else~(\rule@name)\fi}}%
{}
\srefaddidkey{statement}
\newenvironment{statement}[1][]{\metasetkeys{statement}{#1}}{}
\srefaddidkey{proof}
\newenvironment{proof}[1][]{\metasetkeys{proof}{#1}}{}
\srefaddidkey{definition}
\addmetakey{definition}{term}
\addmetakey{definition}{seealso}
\newenvironment{definition}[1][]{\metasetkeys{definition}{#1}{\noindent\bfseries{Definition:}}}{}
\srefaddidkey{meaning}
\newenvironment{cmeaning}[1][]{\metasetkeys{meaning}{#1}}{}
\addmetakey{cnxn}{document}
\addmetakey{cnxn}{target}
\addmetakey{cnxn}{strength}
\newcommand\cnxn[2][]% keys, link text
{\metasetkeys{cnxn}{#1}{\underline{#2}}\footnote{{\ttfamily\@ifx\cnxn@document\@empty\cnxn@document\fi\#\cnxn@target}}}
\newcommand\@makefntext[1]{\parindent 1em\noindent\hb@xt@1.8em{\hss\@makefnmark}#1}
\addmetakey{link}{src}
\newcommand\link[2][]{\metasetkeys{link}{#1}\underline{#2}}
\srefaddidkey{cfigure}
\addmetakey{cfigure}{type}
\addmetakey{cfigure}{caption}
\newcounter{figure}
\newcommand\cfigure[3][]{% cnx_keys, graphicx_keys, path
\begin{center}%
\includegraphics[#2]{#3}%
\metasetkeys{cfigure}{#1}\sref@target%
\ifx\cfigure@caption\@empty\else
\par\noindent Figure\refstepcounter{figure} {\arabic{figure}}: \cfigure@caption%
\protected@edef\@currentlabel{\arabic{figure}}%
\sref@label@id{Figure \thefigure}\fi
\end{center}}
\addmetakey{ccite}{src}
\newcommand\ccite[2][]{\metasetkeys{ccite}{#1}\emph{#2}}
\newcommand\term[1]{{\bfseries\underline{#1}}}
\addmetakey{metadata}{version}
\addmetakey{metadata}{created}
\addmetakey{metadata}{revised}
\newsavebox{\metadatabox}
\newenvironment{metadata}[1][]%
{\noindent\hfill\begin{lrbox}{\metadatabox}
\begin{minipage}{.8\textwidth}%
{\Large\bfseries CNX Module: \cnx@name\hfill\strut}\\[2ex]}%
{\end{minipage}\end{lrbox}\fbox{\usebox\metadatabox}\hfill}
\newenvironment{authorlist}{{\bfseries{Authors}:~}}{\\[1ex]}
\newenvironment{maintainerlist}{{\bfseries{Maintainers}:~}}{\\[1ex]}
\srefaddidkey{auth}
\addmetakey{auth}{honorific}
\addmetakey{auth}{firstname}
\addmetakey{auth}{other}
\addmetakey{auth}{surname}
\addmetakey{auth}{lineage}
\addmetakey{auth}{email}
\newcommand\cnxauthor[1][]{\metasetkeys{auth}{#1}\auth@first~\auth@sur,}
\newcommand\maintainer[1][]{\metasetkeys{auth}{#1}\auth@first~\auth@sur,}
\newenvironment{keywordlist}{\bfseries{Keywords}:~}{\\[1ex]}
\newcommand\keyword[1]{#1,}
\newenvironment{cnxabstract}%
{\par\noindent\strut\hfill\begin{minipage}{10cm}{\bfseries{Abstract}:~}}%
{\end{minipage}\hfill}
\endinput
%%
%% End of file `cnx.cls'.
