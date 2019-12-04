plugins {
    scala
    `java-library`
}

repositories {
    jcenter()
}


tasks.withType<ScalaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
}

dependencies {
    // Use Scala 2.12 in our library project
    implementation("org.scala-lang:scala-library:2.12.9")

    // Use Scalatest for testing our library
    testImplementation("junit:junit:4.12")
    testImplementation("org.scalatest:scalatest_2.12:3.0.8")

    // Need scala-xml at test runtime
    testRuntimeOnly("org.scala-lang.modules:scala-xml_2.12:1.2.0")
}
