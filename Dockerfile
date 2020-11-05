FROM openjdk:13-alpine
COPY ./* /app/
WORKDIR /app/
RUN javac -d ./output ./MyTest.java
WORKDIR /app/output