package com.sgm.navi.service.define.aos;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [在这里描述文件功能]
 */
public class FyGTrifficSocolPicture implements Parcelable {
    public double x;
    public double y;
    public String socol_picture;

    protected FyGTrifficSocolPicture(Parcel in) {
        x = in.readDouble();
        y = in.readDouble();
        socol_picture = in.readString();
    }

    public static final Creator<FyGTrifficSocolPicture> CREATOR = new Creator<FyGTrifficSocolPicture>() {
        @Override
        public FyGTrifficSocolPicture createFromParcel(Parcel in) {
            return new FyGTrifficSocolPicture(in);
        }

        @Override
        public FyGTrifficSocolPicture[] newArray(int size) {
            return new FyGTrifficSocolPicture[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeDouble(x);
        dest.writeDouble(y);
        dest.writeString(socol_picture);
    }
}
