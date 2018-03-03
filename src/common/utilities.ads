with GL.Objects;
with GL.Objects.Buffers;

with GL.Types;
with GL.Types.Colors;


package Utilities is

   procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color);
   
   procedure Map is new
     GL.Objects.Buffers.Map(GL.Types.Singles.Matrix4_Pointers);
   
   procedure Load_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.UShorts.Vector3_Pointers);
   
   procedure Load_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.UInt_Pointers);
   
   procedure Load_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.UShort_Pointers);
   
   procedure Load_To_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.Singles.Matrix4_Pointers);
   
   procedure Load_Vertex_Buffer_2 is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.Singles.Vector2_Pointers);
   procedure Load_Vertex_Buffer_3 is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.Singles.Vector3_Pointers);
   procedure Load_Vertex_Buffer_4 is new
     GL.Objects.Buffers.Load_To_Buffer(GL.Types.Singles.Vector4_Pointers);
   
   function Pointer is new
     GL.Objects.Buffers.Pointer(GL.Types.Singles.Matrix4_Pointers);
   
   procedure Set_Buffer_Sub_Data_M is new
     GL.Objects.Buffers.Set_Sub_Data(GL.Types.Singles.Matrix4_Pointers);
   procedure Set_Buffer_Sub_Data is new
     GL.Objects.Buffers.Set_Sub_Data(GL.Types.Singles.Vector3_Pointers);
   procedure Set_Buffer_Sub_Data is new
     GL.Objects.Buffers.Set_Sub_Data(GL.Types.Singles.Vector4_Pointers);
   
   
   
   procedure Show_GL_Data;
     
   function Get_Size is new
     GL.Objects.Buffers.Get_Bytes_Count(GL.Types.Singles.Vector3_Pointers);
   function Get_Size is new
     GL.Objects.Buffers.Get_Bytes_Count(GL.Types.Singles.Vector4_Pointers);
   
end Utilities;
