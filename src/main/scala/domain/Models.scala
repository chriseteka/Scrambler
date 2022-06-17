package com.chrisworks
package domain

import domain.AccessMean.AccessMeans
import domain.AccessProfileConnAccessMean.AccessProfilesConnAccessMeans
import domain.Grantor.Grantors
import domain.IpIpRelationship.IpIpRelationships
import domain.Keys.SharedKeys._

import io.circe.{Json, JsonObject}

/** We shall use these shapes to further characterize a particular type and will use it in generating the graph */
sealed trait Shape

object Shape {

  final case object Unknown extends Shape

  final case object Circle extends Shape

  final case object Square extends Shape

  final case object Triangle extends Shape

  final case object Trapezium extends Shape

  final case object Rectangle extends Shape

  final case object Parallelogram extends Shape
}

sealed abstract class GraphObject(val shape: Shape) {
  def partialBuildGraph(): String
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
  final case object Individual extends EntityType("BLUE")

  final case object Organisation extends EntityType("RED")

  final case object OrganisationUnit extends EntityType("GREEN")

  final case object Unknown extends EntityType("BLACK")

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

  override def partialBuildGraph(): String =
    s"""
       | [$shape: $countryCode] -> [${identifier.produceId}]
       | [${identifier.produceId}] -> [${grantors.map(_.conn).mkString(",")}]
       | [${identifier.produceId}] -> [${grantee.map(_.conn).getOrElse("")}]
       | [${identifier.produceId}] -> [${maybeSuper.map(_.conn).getOrElse("")}]
       |""".stripMargin
}

object IpIpRelationship {

  import JsonObjectEnricher._
  import Keys.RootKeys._
  import Keys.SharedKeys._

  type IpIpRelationships = List[IpIpRelationship]

  def buildRelationshipFrom(data: Json): Option[IpIpRelationship] =
    data.asObject.map(obj =>
      IpIpRelationship(
        IdentifierWithType(obj.extractAsString(ID), obj.extractAsString(TYPE)),
        obj.extractAsString(COUNTRY_CODE),
        obj.extractFromArrayAsConnectionWithType(GRANTORS_KEY, Grantor.shape),
        obj.extractFromObjAsConnectionWithType(GRANTEE_KEY, Grantee.shape).headOption,
        obj.extractFromObjAsConnectionWithType(SUPER_HOLDER, Shape.Unknown).headOption
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
                            defaultId: String) extends GraphObject(Shape.Circle) {

  override def partialBuildGraph(): String =
    s"""
      | [$shape: $defaultId] -> [${identifier.produceId}]
      |""".stripMargin
}

object AccessMean {
  import JsonObjectEnricher._

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
                               ipIpRelations: List[Connection]) extends GraphObject(Shape.Square) {
  override def partialBuildGraph(): String =
    s"""
      | [$shape: $name] -> [${identifier.produceId}]
      | [${identifier.produceId}] -> [${holderId.map(_.conn)}]
      | [${identifier.produceId}] -> [${userId.map(_.conn)}]
      | [${identifier.produceId}] -> [${ipIpRelations.map(_.conn)}]
      |""".stripMargin
}
object AccessProfile {
  import JsonObjectEnricher._

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
                           accessProfileConnAccessMean: Option[AccessProfilesConnAccessMeans]) {

  lazy val relationships: Seq[Serializable] = List(
    grantee.map(_.partialBuildGraph()).getOrElse(""),
    grantors.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse(""),
    ipIpRelationships.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse(""),
    accessMeans.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse(""),
    accessProfileConnAccessMean.map(_.map(_.partialBuildGraph()).mkString(",")).getOrElse("")
  )

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
      SyncGraph(
        grantee,
        grantors,
        ipIpRel,
        accessMeans,
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
    val DEFAULT_ACCESS_PROFILE = "defaultAccessProfileId"
  }

}

final case class JsonObjectEnricher(obj: JsonObject) {
  import JsonEnricher.toEnricher

  def extractAsString(key: String): String = obj(key).map(_.toString()).getOrElse("")
    .replaceAll("[^a-zA-Z\\d]+","")

  private val readConnectionWithTypeFrom: (JsonObject, Shape) => Option[List[ConnectionWithType]] = (jsonObject, shape) => {
    val oType = jsonObject(TYPE).map(_.toString()).getOrElse("")
    jsonObject(ID)
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


  def extractFromArrayAsConnectionWithType(key: String, shape: Shape): List[ConnectionWithType] = {
    (for {
      s1 <- obj(key)
      s2 <- s1.asArray.map(_.toList.flatMap(_.asObject))
    } yield s2.flatMap(obj => readConnectionWithTypeFrom(obj, shape))).getOrElse(List.empty).flatten
  }

  def extractFromObjAsConnectionWithType(key: String, shape: Shape): List[ConnectionWithType] = {
    (for {
      s1 <- obj(key)
      s2 <- s1.asObject
    } yield s2).flatMap(obj => readConnectionWithTypeFrom(obj, shape)).getOrElse(List.empty)
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

