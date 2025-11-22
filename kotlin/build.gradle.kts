plugins {
    kotlin("multiplatform") version "2.1.21"
}

repositories {
    mavenCentral()
}

kotlin {
    // Examples can be executed with JVM or natively for macOS, Linux, or Windows
    jvm()
    macosArm64()
    linuxX64()
    mingwX64()

    applyDefaultHierarchyTemplate()
    sourceSets {
        val commonMain by getting {
            dependencies {
                api("dev.kourier:amqp-client:0.3.1")
            }
        }
        val commonTest by getting {
            dependencies {
                implementation(kotlin("test"))
            }
        }
    }
}
