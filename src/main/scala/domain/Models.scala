package com.chrisworks
package domain

import domain.AccessMean.AccessMeans
import domain.AccessProfile.AccessProfiles
import domain.AccessProfileConnAccessMean.AccessProfilesConnAccessMeans
import domain.Grantor.Grantors
import domain.IpIpRelationship.IpIpRelationships
import domain.Keys.SharedKeys._

import com.chrisworks.domain.EntityType.Organisation
import io.circe.{Json, JsonObject}

/** We shall use these shapes to further characterize a particular type and will use it in generating the graph */
sealed abstract class Shape(val lbl: String)

object Shape {

  final case object Unknown extends Shape("Unk")

  final case object Oval extends Shape("Ova")

  final case object Diamond extends Shape("Dia")

  final case object Triangle extends Shape("Tri")

  final case object Trapezium extends Shape("Tra")

  final case object Rectangle extends Shape("Rec")

  final case object Parallelogram extends Shape("Par")
}

sealed abstract class GraphObject(val shape: Shape) {

  /** This gives a description of the graph, by showing the structure/style as String */
  def partialBuildGraph(): String

  /** This is the actual graph which is in fact what graphviz can turn into an understandable image */
  def materializeGraph(): String

  /** This allows an object point to how it's key/id relates with some other objects within it's context or reach */
  def relatesWith(): String
}

/** This defines a simple connection by stating the key on which the connection is made to and the UUID the key holds */
final case class Connection(connKey: String, value: String) {

  def conn: String = s"[$connKey: $value]"
}

/** This class is used to describe a connection from an entity to another, where few data are extracted from each entity. The shape field helps tell us which object this connection was made to */
final case class ConnectionWithType(connection: Connection, oType: String, shape: Shape) {

  def conn: String =
    if (oType.isBlank) s"$shape: ${connection.conn}"
    else s"$shape: ${connection.conn} - $oType"
}

/** This defines how a typical identifier is represented */
final case class IdentifierWithType(id: String, oType: String = "") {

  def produceId: String = if (oType.isBlank) id else s"$id - $oType"
}

/** We attempt to use this in describing an entity (Grantor, and Grantee) */
sealed abstract class EntityType(val color: String)

object EntityType {
  final case object Individual extends EntityType("deepskyblue")

  final case object Organisation extends EntityType("gold")

  final case object OrganisationUnit extends EntityType("coral")

  final case object Unknown extends EntityType("red")

  def from(str: String): EntityType =
    List(Individual, Organisation, OrganisationUnit)
      .find(_.toString.toLowerCase.equalsIgnoreCase(str.toLowerCase))
      .getOrElse(Unknown)
}

final case class Grantee(identifier: IdentifierWithType,
                         countryCode: String,
                         entityType: EntityType) extends GraphObject(Shape.Triangle) {

  override def partialBuildGraph(): String =
    s"[$shape: ${entityType.color}: $countryCode] -> [${identifier.produceId}]"

  override def materializeGraph(): String =
    "%s%s [margin=0 fillcolor=%s shape=%s style=\"rounded,filled\" label=\"%s, %s\"]"
      .format(shape.lbl, identifier.id, entityType.color, shape.toString.toLowerCase, countryCode, identifier.produceId)

  override def relatesWith(): String = "" //Relates with no one, as it is sort of the base object that other point to
}

object Grantee {
  import JsonObjectEnricher.toEnricher
  import Keys.SharedKeys._

  val shape: Shape = Shape.Triangle

  def asEntity(entityType: String, obj: JsonObject): Grantee =
    Grantee(
      IdentifierWithType(obj.extractAsString(ID)),
      obj.extractAsString(COUNTRY_CODE),
      EntityType.from(entityType)
    )

  def buildEntityFrom(data: (String, Json)): Option[Grantee] =
    data._2.asObject.map(obj => asEntity(data._1, obj))
}

final case class Grantor(identifier: IdentifierWithType,
                         countryCode: String,
                         entityType: EntityType) extends GraphObject(Shape.Rectangle) {

  override def partialBuildGraph(): String =
    s"[$shape: ${entityType.color}: $countryCode] -> [${identifier.produceId}]"

  override def materializeGraph(): String =
    "%s%s [margin=0 fillcolor=%s shape=%s style=\"rounded,filled\" label=\"%s, %s\"]"
      .format(shape.lbl, identifier.id, entityType.color, shape.toString.toLowerCase, countryCode, identifier.produceId)

  override def relatesWith(): String = "" //Relates with no one, as it is sort of the base object that other point to
}

object Grantor {
  import JsonObjectEnricher.toEnricher
  import Keys.SharedKeys._

  val shape: Shape = Shape.Rectangle
  type Grantors = List[Grantor]

  def asEntity(entityType: String, obj: JsonObject): Grantor =
    Grantor(
      IdentifierWithType(obj.extractAsString(ID)),
      obj.extractAsString(COUNTRY_CODE),
      EntityType.from(entityType)
    )

  def buildEntityFrom(data: (String, Json)): Option[Grantor] =
    data._2.asObject.map(obj => asEntity(data._1, obj))

  def buildEntityFrom(data: List[(String, Json)]): Option[Grantors] =
    Some(data.foldLeft(List.empty[Grantor]) { (res, next) =>
      buildEntityFrom(next) match {
        case Some(value) => res :+ value
        case None => res
      }
    })
}

/** Description to what an IpIpRelationship is */
final case class IpIpRelationship(identifier: IdentifierWithType,
                                  countryCode: String,
                                  grantors: List[ConnectionWithType],
                                  grantee: Option[ConnectionWithType],
                                  maybeSuper: Option[ConnectionWithType])
  extends GraphObject(Shape.Trapezium) {

  val possibleSuperHolder: String = if (maybeSuper.isEmpty) ""
  else "%s%s [margin=0 fillcolor=%s shape=%s style=\"rounded,filled\" label=\"%s: %s\"]"
    .format(Grantor.shape.lbl, maybeSuper.get.connection.value, Organisation.color,
      Grantor.shape.toString.toLowerCase, maybeSuper.get.connection.connKey, maybeSuper.get.connection.value)

  override def partialBuildGraph(): String =
    s"""
       | [$shape: $countryCode] -> [${identifier.produceId}]
       | [${identifier.produceId}] -> [${grantors.map(_.conn).mkString(",")}]
       | [${identifier.produceId}] -> [${grantee.map(_.conn).getOrElse("")}]
       | [${identifier.produceId}] -> [${maybeSuper.map(_.conn).getOrElse("")}]
       |""".stripMargin

  override def materializeGraph(): String =
    "%s%s [margin=0 fillcolor=gray shape=%s style=\"rounded,filled\" label=\"%s, %s\"]"
      .format(shape.lbl, identifier.id, shape.toString.toLowerCase, countryCode, identifier.produceId)

  override def relatesWith(): String = {
    val makePointerGrantee: Option[ConnectionWithType] => String = optional =>
      if (optional.nonEmpty) s"${shape.lbl}${identifier.id} -> ${optional.map(c => c.shape.lbl + c.connection.value).get} [minlen=3, penwidth=2]" else ""

    val makePointerSuperHolder: Option[ConnectionWithType] => String = optional =>
      if (optional.nonEmpty) s"${optional.map(c => c.shape.lbl + c.connection.value).get} -> ${shape.lbl}${identifier.id} [minlen=3, penwidth=2]" else ""

    s"""
       | ${grantors.map(grantor => shape.lbl + identifier.id + " -> " + grantor.shape.lbl + grantor.connection.value + " [minlen=3, penwidth=2]").mkString("\n")}
       | ${makePointerGrantee(grantee)}
       | ${makePointerSuperHolder(maybeSuper)}
       |""".stripMargin
  }
}

object IpIpRelationship {

  import JsonObjectEnricher._
  import Keys.RootKeys._
  import Keys.SharedKeys._

  val shape: Shape = Shape.Trapezium
  type IpIpRelationships = List[IpIpRelationship]

  def buildRelationshipFrom(data: Json): Option[IpIpRelationship] =
    data.asObject.map(obj =>
      IpIpRelationship(
        IdentifierWithType(obj.extractAsString(ID), obj.extractAsString(TYPE)),
        obj.extractAsString(COUNTRY_CODE),
        obj.extractFromArrayAsConnectionWithType(GRANTORS_KEY, Grantor.shape),
        obj.extractFromObjAsConnectionWithType(GRANTEE_KEY, Grantee.shape).headOption,
        obj.extractFromObjAsConnectionWithType(SUPER_HOLDER, Grantor.shape, ORGANISATION).headOption
      )
    )

  def buildRelationshipFrom(data: List[Json]): Option[IpIpRelationships] =
    Some(data.foldLeft(List.empty[IpIpRelationship]) { (res, next) =>
      buildRelationshipFrom(next) match {
        case Some(value) => res :+ value
        case None => res
      }
    })

}

/** An access mean has more than this, but we only care to know how to identifier it, also we tag it as a graph object */
final case class AccessMean(identifier: IdentifierWithType,
                            defaultId: String) extends GraphObject(Shape.Oval) {

  override def partialBuildGraph(): String =
    s"""
      | [$shape: $defaultId] -> [${identifier.produceId}]
      |""".stripMargin

  override def materializeGraph(): String =
    "%s%s [margin=0 fillcolor=pink shape=%s style=\"rounded,filled\" label=\"%s\"]"
      .format(shape.lbl, identifier.id, shape.toString.toLowerCase, identifier.produceId)

  override def relatesWith(): String = "" //Relates with no one, as it is sort of the base object that other point to
}

object AccessMean {
  import JsonObjectEnricher._

  val shape: Shape = Shape.Oval
  type AccessMeans = List[AccessMean]

  def buildAccessMeanFrom(data: Json): Option[AccessMean] =
    data.asObject.map(obj =>
      AccessMean(
        IdentifierWithType(obj.extractAsString(ID), obj.extractAsString(TYPE)),
        obj.extractAsString(DEFAULT_ACCESS_PROFILE)
      )
    )

  def buildAccessMeanFrom(data: List[Json]): Option[AccessMeans] =
    Some(data.foldLeft(List.empty[AccessMean]) { (res, next) =>
      buildAccessMeanFrom(next) match {
        case Some(value) => res :+ value
        case None => res
      }
    })
}

/** An access profile has more data, but these are the sides to it that are of importance, also tagged as graph object */
final case class AccessProfile(identifier: IdentifierWithType,
                               name: String,
                               holderId: Option[Connection],
                               userId: Option[Connection],
                               ipIpRelations: List[Connection]) extends GraphObject(Shape.Diamond) {
  override def partialBuildGraph(): String =
    s"""
      | [$shape: $name] -> [${identifier.produceId}]
      | [${identifier.produceId}] -> [${holderId.map(_.conn)}]
      | [${identifier.produceId}] -> [${userId.map(_.conn)}]
      | [${identifier.produceId}] -> [${ipIpRelations.map(_.conn)}]
      |""".stripMargin

  override def materializeGraph(): String =
    "%s%s [margin=0 fillcolor=aquamarine3 shape=%s style=\"rounded,filled\" label=\"%s, %s\"]"
      .format(shape.lbl, identifier.id, shape.toString.toLowerCase, name, identifier.produceId)

  override def relatesWith(): String = {
    val makePointer: (Option[Connection], String) => String = (optional, color) =>
      if (optional.nonEmpty) s"${shape.lbl}${identifier.id} -> ${optional.map(c => c.connKey + c.value).get} [minlen=3, penwidth=2, color=$color]" else ""
    s"""
       | ${makePointer(holderId, "red")}
       | ${makePointer(userId, "blue")}
       | ${ipIpRelations.filterNot(_.value.isBlank).map(ipIp => shape.lbl + identifier.id + " -> " + IpIpRelationship.shape.lbl + ipIp.value + " [minlen=3, penwidth=2]").mkString("\n")}
       |""".stripMargin
  }
}
object AccessProfile {
  import JsonObjectEnricher._

  type AccessProfiles = List[AccessProfile]

  def buildAccessProfileFrom(data: Json): Option[AccessProfile] =
    data.asObject.map(obj =>
      AccessProfile(
        IdentifierWithType(obj.extractAsString(ID), obj.extractAsString(TYPE)),
        obj.extractAsString(NAME),
        obj.extractFromObjectAsConnection(HOLDER_ID),
        obj.extractFromObjectAsConnection(USER_ID),
        obj.extractFromObjectAsConnections(RELATIONSHIPS)
      )
    )
}

/** This describes the connection between access profile and access means, it's more like a Wrapper hence a graph */
final case class AccessProfileConnAccessMean(accessProfile: AccessProfile, accessMeans: AccessMeans) extends GraphObject(Shape.Parallelogram) {
  override def partialBuildGraph(): String =
    s"""
       | [$shape: ${accessProfile.identifier.id}] -> [${accessMeans.map(_.partialBuildGraph()).mkString(",")}]
       |""".stripMargin

  /** This is the actual graph which is in fact what graphviz can turn into an understandable image */
  override def materializeGraph(): String =
    "%s%s [margin=0 fillcolor=orange shape=%s style=\"rounded,filled\" label=\"%s, %s\"]"
      .format(shape.lbl, accessProfile.identifier.id, shape.toString.toLowerCase, accessProfile.name, accessProfile.identifier.produceId)

  /** This allows an object point to how it's key/id relates with some other objects within it's context or reach */
  override def relatesWith(): String = accessMeans.map(_.identifier.id)
    .map(id => s"${AccessMean.shape.lbl}$id -> ${accessProfile.shape.lbl}${accessProfile.identifier.id} [dir=none, minlen=3, penwidth=2]")
    .mkString("\n")
}

object AccessProfileConnAccessMean {

  type AccessProfilesConnAccessMeans = List[AccessProfileConnAccessMean]

  def buildProfileConnMeanFrom(data: (Json, Json)): Option[AccessProfileConnAccessMean] =
    for {
      profile <- AccessProfile.buildAccessProfileFrom(data._1)
      means <- data._2.asArray.map(_.toList).flatMap(AccessMean.buildAccessMeanFrom)
    } yield AccessProfileConnAccessMean(profile, means)

  def buildProfileConnMeanFrom(data: List[(Json, Json)]): Option[AccessProfilesConnAccessMeans] =
    Some(data.foldLeft(List.empty[AccessProfileConnAccessMean]) { (res, next) =>
      buildProfileConnMeanFrom(next) match {
        case Some(value) => res :+ value
        case None => res
      }
    })
}

/** This is a description of how our file is formed in the end, everything minimizes to */
final case class SyncGraph(grantee: Option[Grantee],
                           grantors: Option[Grantors],
                           ipIpRelationships: Option[IpIpRelationships],
                           accessMeans: Option[AccessMeans],
                           accessProfiles: Option[AccessProfiles],
                           accessProfileConnAccessMean: Option[AccessProfilesConnAccessMeans]) {

  def returnDataOrEmptyStr(optionalData: Option[String]): String = optionalData.getOrElse("")

  lazy val relationships: Seq[Serializable] = List(
    grantee.map(_.partialBuildGraph()).getOrElse(""),
    grantors.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse(""),
    ipIpRelationships.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse(""),
    accessMeans.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse(""),
    accessProfileConnAccessMean.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse("")
  )

  lazy val materializeRelationships: String =
    s"""
      |digraph ProfileSyncMat {
      | {
      |   \t${returnDataOrEmptyStr(grantee.map(_.materializeGraph()))}
      |   \t${returnDataOrEmptyStr(grantors.map(_.map(_.materializeGraph()).mkString("\n\t\t")))}
      |   \t${returnDataOrEmptyStr(ipIpRelationships.map(_.map(_.possibleSuperHolder).mkString("\n\t\t")))}
      |   \t${returnDataOrEmptyStr(accessMeans.map(_.map(_.materializeGraph()).mkString("\n\t\t")))}
      |   \t${returnDataOrEmptyStr(ipIpRelationships.map(_.map(_.materializeGraph()).mkString("\n\t\t")))}
      |   \t${returnDataOrEmptyStr(accessProfiles.map(_.map(_.materializeGraph()).mkString("\n\t\t")))}
      | }
      |  \t${returnDataOrEmptyStr(ipIpRelationships.map(_.map(_.relatesWith()).mkString("\n\t\t")))}
      |  \t${returnDataOrEmptyStr(accessProfiles.map(_.map(_.relatesWith()).mkString("\n\t\t")))}
      |  \t${returnDataOrEmptyStr(accessProfileConnAccessMean.map(_.map(_.relatesWith()).mkString("\n\t\t")))}
      |}
      |""".stripMargin

  def buildGraph(): String = relationships
    .mkString("digraph ProfileSync {\n\t", "\n\t", "\n}")
    .replaceAll("\n,", ",")
}

object SyncGraph {

  import JsonEnricher._
  import Keys.RootKeys._

  /** Given a JSON, we can produce this object as follows */
  def fromJson(root: Json): Option[SyncGraph] = {
    root.asObject.map(obj => {
      val grantee: Option[Grantee] = obj(GRANTEE_KEY).flatMap(_.toGrantee)
      val grantors: Option[Grantors] = obj(GRANTORS_KEY).flatMap(_.toGrantors)
      val ipIpRel: Option[IpIpRelationships] = obj(IP_IP_REL_KEY).flatMap(_.toIpIpRelationships)
      val accessMeans: Option[AccessMeans] = obj(ACCESS_MEANS_KEY).flatMap(_.toAccessMeans)
      val accProfsConAccMeans: Option[AccessProfilesConnAccessMeans] = obj(ACC_PROF_CONN_ACC_MEAN_KEY).flatMap(_.toProfilesConnAccMeans)
      val accessProfiles: Option[AccessProfiles] = accProfsConAccMeans.map(_.map(_.accessProfile))
      SyncGraph(
        grantee,
        grantors,
        ipIpRel,
        accessMeans,
        accessProfiles,
        accProfsConAccMeans
      )
    })
  }
}

object Keys {

  object RootKeys {
    val GRANTEE_KEY = "grantee"
    val GRANTORS_KEY = "grantors"
    val IP_IP_REL_KEY = "ipipRelationships"
    val ACCESS_MEANS_KEY = "allAccessMeans"
    val ACC_PROF_CONN_ACC_MEAN_KEY = "connectedAccessProfilesWithAccessMeans"
  }

  object SharedKeys {
    val ID = "id"
    val TYPE = "type"
    val NAME = "name"
    val USER_ID = "userId"
    val HOLDER_ID = "holderId"
    val COUNTRY_CODE = "countryCode"
    val RELATIONSHIPS = "relationships"
    val SUPER_HOLDER = "maybeSuperHolder"
    val ORGANISATION = "Organisation"
    val DEFAULT_ACCESS_PROFILE = "defaultAccessProfileId"
  }

}

final case class JsonObjectEnricher(obj: JsonObject) {
  import JsonEnricher.toEnricher

  def extractAsString(key: String): String = obj(key).map(_.toString()).getOrElse("")
    .replaceAll("[^a-zA-Z\\d]+","")

  private val readConnectionWithTypeFrom: (JsonObject, Shape, String) => Option[List[ConnectionWithType]] = (jsonObject, shape, root) => {
    val oType = jsonObject(TYPE).map(_.toString()).getOrElse("")
    jsonObject(root)
      .flatMap(_.asObject)
      .map(_.toIterable
        .foldLeft(List.empty[ConnectionWithType]) { case (res, next) =>
          res :+ ConnectionWithType(Connection(next._1, next._2.turnToString), oType, shape)
        }
      )
  }

  def extractFromObjectAsConnection(key: String): Option[Connection] =
    obj(key)
      .flatMap(_.asObject)
      .flatMap(_.toIterable
        .headOption
        .map { case (str, json) => Connection(str, json.turnToString) }
      )

  def extractFromObjectAsConnections(key: String): List[Connection] =
    obj(key)
      .flatMap(_.asArray)
      .map(_.flatMap(_.asObject))
      .map(_.flatMap(_.toIterable)
        .foldLeft(List.empty[Connection]) { (res, next) =>
          res :+ Connection(next._1, next._2.turnToString)
        }
      ).getOrElse(List.empty)


  def extractFromArrayAsConnectionWithType(key: String, shape: Shape, root: String = ID): List[ConnectionWithType] = {
    (for {
      s1 <- obj(key)
      s2 <- s1.asArray.map(_.toList.flatMap(_.asObject))
    } yield s2.flatMap(obj => readConnectionWithTypeFrom(obj, shape, root))).getOrElse(List.empty).flatten
  }

  def extractFromObjAsConnectionWithType(key: String, shape: Shape, root: String = ID): List[ConnectionWithType] = {
    (for {
      s1 <- obj(key)
      s2 <- s1.asObject
    } yield s2).flatMap(obj => readConnectionWithTypeFrom(obj, shape, root)).getOrElse(List.empty)
  }

}

object JsonObjectEnricher {
  implicit val toEnricher: JsonObject => JsonObjectEnricher = JsonObjectEnricher(_)
}

final case class JsonEnricher(json: Json) {

  /** Here the first element is a string, and the second element is a JSON */
  lazy val convertToArrayWithObjectsSplitKeys: Option[List[(String, Json)]] =
    json.asArray.map(_.toList.flatMap(_.asObject).flatMap(_.toIterable))

  /** First element of the tuple is an object, second element is an array */
  lazy val convertToArrayWithArraysSplitIndexes: Option[List[(Json, Json)]] =
    json.asArray.map(_.toList.flatMap(_.asArray).map(a => (a(0), a(1))))

  def turnToString: String = json.asString.getOrElse("")
    .replaceAll("[^a-zA-Z\\d]+","")

  def toGrantee: Option[Grantee] = json.asObject.flatMap(e => Grantee.buildEntityFrom(e.toList.head))

  def toGrantors: Option[Grantors] = convertToArrayWithObjectsSplitKeys.flatMap(Grantor.buildEntityFrom)

  def toIpIpRelationships: Option[IpIpRelationships] = json.asArray.map(_.toList).flatMap(IpIpRelationship.buildRelationshipFrom)

  def toAccessMeans: Option[AccessMeans] = json.asArray.map(_.toList).flatMap(AccessMean.buildAccessMeanFrom)

  def toProfilesConnAccMeans: Option[AccessProfilesConnAccessMeans] = convertToArrayWithArraysSplitIndexes.flatMap(AccessProfileConnAccessMean.buildProfileConnMeanFrom)
}

object JsonEnricher {
  implicit val toEnricher: Json => JsonEnricher = JsonEnricher(_)
}

