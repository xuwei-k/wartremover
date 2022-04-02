package org.wartremover;

public interface InspectResult {
  WartDiagnostic[] warnings();
  WartDiagnostic[] errors();

  public static InspectResult empty() {
    return new InspectResult() {
      public WartDiagnostic[] warnings() { 
        return new WartDiagnostic[0];
      }
      public WartDiagnostic[] errors(){ 
        return new WartDiagnostic[0];
      }
    };
  }
}
