package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RouteRestrictionInfo implements Parcelable {
    private String mTitle;
    private String mDesc;
    private String mTips;
    private int mCityCode;
    private short mType;
    private short mTitleType;
    private ArrayList<Long> mRuleIDs;
    private ArrayList<Short> mTailNums;

    protected RouteRestrictionInfo(Parcel in) {
        mTitle = in.readString();
        mDesc = in.readString();
        mTips = in.readString();
        mCityCode = in.readInt();
        mType = (short) in.readInt();
        mTitleType = (short) in.readInt();
    }

    public static final Creator<RouteRestrictionInfo> CREATOR = new Creator<RouteRestrictionInfo>() {
        @Override
        public RouteRestrictionInfo createFromParcel(Parcel in) {
            return new RouteRestrictionInfo(in);
        }

        @Override
        public RouteRestrictionInfo[] newArray(int size) {
            return new RouteRestrictionInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(mTitle);
        dest.writeString(mDesc);
        dest.writeString(mTips);
        dest.writeInt(mCityCode);
        dest.writeInt((int) mType);
        dest.writeInt((int) mTitleType);
    }
}
