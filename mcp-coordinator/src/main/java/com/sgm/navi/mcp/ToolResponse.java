package com.sgm.navi.mcp;

import android.os.Parcel;
import android.os.Parcelable;

public class ToolResponse implements Parcelable {
    private String toolName;
    private String result;
    private boolean success;

    public ToolResponse(String toolName, String result, boolean success) {
        this.toolName = toolName;
        this.result = result;
        this.success = success;
    }

    protected ToolResponse(Parcel in) {
        toolName = in.readString();
        result = in.readString();
        success = in.readByte() != 0;
    }

    public static final Creator<ToolResponse> CREATOR = new Creator<ToolResponse>() {
        @Override
        public ToolResponse createFromParcel(Parcel in) {
            return new ToolResponse(in);
        }

        @Override
        public ToolResponse[] newArray(int size) {
            return new ToolResponse[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(toolName);
        dest.writeString(result);
        dest.writeByte((byte) (success ? 1 : 0));
    }

    public String getToolName() {
        return toolName;
    }

    public String getResult() {
        return result;
    }

    public boolean isSuccess() {
        return success;
    }
}