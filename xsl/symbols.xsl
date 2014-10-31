<?xml version="1.0" encoding="utf-8"?>
<!-- An XSL style sheet for transforming LaTeXML symbols to OMDoc (Open Mathematical Documents). 
     $Id: symbols.xsl 1858 2011-08-30 16:14:23Z kohlhase $
     $HeadURL: https://svn.kwarc.info/repos/stex/branches/stex+xhtml/xsl/symbols.xsl $
     send bug-reports, patches, suggestions to users@omdoc.org or developers@omdoc.org 

     Copyright (c) 2000 - 2013 Michael Kohlhase, 

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2.1 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://omdoc.org/ns"
  xmlns:omdoc="http://omdoc.org/ns"
  xmlns:ltx="http://dlmf.nist.gov/LaTeXML"
  xmlns:om="http://www.openmath.org/OpenMath"
  exclude-result-prefixes="xsl omdoc ltx om">

<xsl:output method="xml" indent="yes" cdata-section-elements="data"/>

<!-- these parameters set the paths to the special latexml cds. The default is made so that it works for MathHub -->
<xsl:param name="smglom" select="'smglom'"/>
<xsl:param name="numberfields" select="concat($smglom,'/numberfields/source')"/>
<xsl:param name="mv" select="concat($smglom,'/mv/source')"/>
<xsl:param name="sets" select="concat($smglom,'/sets/source')"/>
<xsl:param name="calculus" select="concat($smglom,'/calculus/source')"/>

<!-- get rid of the list OMAs LaTeXML uses -->
<xsl:template match="om:OMA[om:OMS[position()=1 and @name='list' and @cd='latexml']]">
  <xsl:apply-templates select="*[position() &gt; 1]"/>
</xsl:template>

<!-- special treatment for latexml symbols -->
<!-- we have a set of special CDs that correspond to the ones latexml postulates -->
<!-- they need to be imported whereever necessary -->
<xsl:template match="omdoc:theory">
  <omdoc:theory>
    <xsl:apply-templates select="@*"/>
    <xsl:if test="//om:OMS[@cd='latexml' and @name='multirelation' and
		                           not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$sets}/multirel#multirel"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		            (@name='times' or
		             @name='divide' or
		             @name='plus' or
		             @name='minus' or
		             @name='square-root')
			     and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$numberfields}/arithmetics#arithmetics"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		            (@name='greater-than' or
 		             @name='less-than' or
		             @name='greater-than-or-equals' or
		             @name='less-than-or-equals')
			      and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$numberfields}/numbers-orders#numbers-orders"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  (@name='not-equals' or
 		  @name='equals') and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$mv}/equal#equal"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='infinity' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$numberfields}/infinity#infinity"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='absolute-value' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$numberfields}/absolutevalue#absolutevalue"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='approximately-equals' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$mv}/approxeq#approxeq"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='assign' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$mv}/defeq#defeq"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='compose' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$sets}/relation-composition#relation-composition"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='element-of' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$sets}/set#set"/>
    </xsl:if>
    <xsl:if test="//om:OMS[@cd='latexml' and 
		  @name='natural-logarithm' and not(ancestor::omdoc:notation)]">
      <omdoc:imports from="{$calculus}/naturallogarithm#naturallogarithm"/>
    </xsl:if>
    <xsl:apply-templates/>
  </omdoc:theory>
</xsl:template>

<!-- and we need to convert the symbols -->
<xsl:template match="om:OMS[@cd='latexml' and @name='multirelation']">
  <om:OMS cd="multirel" name="multi-relation-expression"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='plus']">
  <om:OMS cd="arithmetics" name="addition"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='minus']">
  <om:OMS cd="arithmetics" name="subtraction"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='times']">
  <om:OMS cd="arithmetics" name="multiplication"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='divide']">
  <om:OMS cd="arithmetics" name="division"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='square-root']">
  <om:OMS cd="arithmetics" name="square-root"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='greater-than']">
  <om:OMS cd="numbers-orders" name="morethan"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='greater-than-or-equals']">
  <om:OMS cd="numbers-orders" name="methan"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='less-than']">
  <om:OMS cd="numbers-orders" name="lethan"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='less-than-or-equals']">
  <om:OMS cd="numbers-orders" name="lessthan"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='not-equals']">
  <om:OMS cd="equal" name="notequal"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='equals']">
  <om:OMS cd="equal" name="equal"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='infinity']">
  <om:OMS cd="infinity" name="infinity"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='absolutevalue']">
  <om:OMS cd="absolutevalue" name="absolute-value"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='approximately-equals']">
  <om:OMS cd="approxeq" name="approximately-equal"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='assign']">
  <om:OMS cd="defeq" name="definitional-equation"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='compose']">
  <om:OMS cd="relation-composition" name="composition"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='element-of']">
  <om:OMS cd="set" name="inset"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='latexml' and @name='natural-logarithm']">
  <om:OMS cd="naturallogarithm" name="natural-logarithm"/>
</xsl:template>

<!-- we clean up for variable names in \nappa and friends -->
<xsl:template match="om:OMV[contains(@name,'normal-')]">
  <om:OMV name="{substring-after(@name,'normal-')}"/>
</xsl:template>

</xsl:stylesheet>
