plugins {
    scala
    // `java-library`
}

repositories {
    jcenter()
    mavenCentral()
}

tasks.withType<ScalaCompile>().configureEach {
    options.apply {
        targetCompatibility = JavaVersion.VERSION_1_8.toString()
        sourceCompatibility = JavaVersion.VERSION_1_8.toString()
    }
}

dependencies {
    val scalaVersion = "2.12"

    // Use Scala 2.12 in our library project
    implementation("org.scala-lang:scala-library:2.12.9")
    implementation("org.scalaz:scalaz-core_$scalaVersion:7.3.1")
    implementation("com.chuusai:shapeless_$scalaVersion:2.3.3")
    implementation("org.typelevel:cats_$scalaVersion:0.9.0")

    // Use Scalatest for testing our library
    testImplementation("junit:junit:4.12")
    testImplementation("org.scalatest:scalatest_$scalaVersion:3.0.8")

    // Need scala-xml at test runtime
    testRuntimeOnly("org.scala-lang.modules:scala-xml_$scalaVersion:1.2.0")
}

/*
val scalazVersion = "7.2.8"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "io.reactivex" %% "rxscala" % "0.26.5",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "cats" % "0.9.0"
)

scalacOptions ++= Seq("-feature","-Xlog-implicits")

initialCommands in console := "import scalaz._, Scalaz._, shapeless._, rx.lang.scala._"

 */