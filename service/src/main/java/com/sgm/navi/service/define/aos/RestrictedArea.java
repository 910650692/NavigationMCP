package com.sgm.navi.service.define.aos;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.PreviewParams;

import java.util.ArrayList;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RestrictedArea implements Parcelable {
    private ArrayList<ArrayList<RestrictedAreaDetail>> mRestrictedAreaDetails = new ArrayList<>();
    private ArrayList<String> mCityNames = new ArrayList<>();
    private ArrayList<Integer> mCityPosition = new ArrayList<>();
    private ArrayList<ArrayList<PreviewParams.PointD>> mPointList = new ArrayList<>();
    private long mRequestId;

    protected RestrictedArea(Parcel in) {
        mCityNames = in.createStringArrayList();
        mRequestId = in.readLong();
    }

    public static final Creator<RestrictedArea> CREATOR = new Creator<RestrictedArea>() {
        @Override
        public RestrictedArea createFromParcel(Parcel in) {
            return new RestrictedArea(in);
        }

        @Override
        public RestrictedArea[] newArray(int size) {
            return new RestrictedArea[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeStringList(mCityNames);
        dest.writeLong(mRequestId);
    }
}
