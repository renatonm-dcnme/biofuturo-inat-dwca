<?xml version="1.0" encoding="UTF-8"?>
<eml:eml packageId="{PACKAGE_ID}" system="https://github.com/{GITHUB_REPO}" xmlns:eml="eml://ecoinformatics.org/eml-2.1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="eml://ecoinformatics.org/eml-2.1.1 https://eml.ecoinformatics.org/eml-2.1.1/eml.xsd">
  <dataset>
    <title xml:lang="{LANG}">{TITLE}</title>
    <creator>
      <individualName>
        <surName>{CREATOR_SURNAME}</surName>
        <givenName>{CREATOR_GIVENNAME}</givenName>
      </individualName>
      <electronicMailAddress>{CREATOR_EMAIL}</electronicMailAddress>
    </creator>
    <metadataProvider>
      <individualName>
        <surName>{CONTACT_SURNAME}</surName>
        <givenName>{CONTACT_GIVENNAME}</givenName>
      </individualName>
      <electronicMailAddress>{CONTACT_EMAIL}</electronicMailAddress>
    </metadataProvider>
    <pubDate>{PUB_DATE}</pubDate>
    <abstract>
      <para>{ABSTRACT}</para>
    </abstract>
    <keywordSet>
      <keyword>citizen science</keyword>
      <keyword>iNaturalist</keyword>
      <keyword>BioBlitz</keyword>
      <keyword>Darwin Core Archive</keyword>
      <keyword>Brazil</keyword>
    </keywordSet>
    <intellectualRights>
      <para>Dataset licensed under {LICENSE_NAME} ({LICENSE_URL}). Individual occurrence records may retain their original iNaturalist license where available.</para>
    </intellectualRights>
    <coverage>
      <geographicCoverage>
        <geographicDescription>{GEO_DESC}</geographicDescription>
        <boundingCoordinates>
          <westBoundingCoordinate>{WEST}</westBoundingCoordinate>
          <eastBoundingCoordinate>{EAST}</eastBoundingCoordinate>
          <northBoundingCoordinate>{NORTH}</northBoundingCoordinate>
          <southBoundingCoordinate>{SOUTH}</southBoundingCoordinate>
        </boundingCoordinates>
      </geographicCoverage>
      <temporalCoverage>
        <rangeOfDates>
          <beginDate>
            <calendarDate>{DATE_BEGIN}</calendarDate>
          </beginDate>
          <endDate>
            <calendarDate>{DATE_END}</calendarDate>
          </endDate>
        </rangeOfDates>
      </temporalCoverage>
    </coverage>
  </dataset>
</eml:eml>
