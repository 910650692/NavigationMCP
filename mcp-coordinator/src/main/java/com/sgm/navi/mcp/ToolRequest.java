package com.sgm.navi.mcp;

import android.os.Parcel;
import android.os.Parcelable;

public class ToolRequest implements Parcelable {
    private String toolName;
    private String parameters;

    public ToolRequest(String toolName, String parameters) {
        this.toolName = toolName;
        this.parameters = parameters;
    }

    protected ToolRequest(Parcel in) {
        toolName = in.readString();
        parameters = in.readString();
    }

    public static final Creator<ToolRequest> CREATOR = new Creator<ToolRequest>() {
        @Override
        public ToolRequest createFromParcel(Parcel in) {
            return new ToolRequest(in);
        }

        @Override
        public ToolRequest[] newArray(int size) {
            return new ToolRequest[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(toolName);
        dest.writeString(parameters);
    }

    public String getToolName() {
        return toolName;
    }

    public String getParameters() {
        return parameters;
    }
}