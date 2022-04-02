package org.wartremover;

public interface WartDiagnostic {
  String message();
  Position position();
}
