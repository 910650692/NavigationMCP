package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RouteMsgPushInfo implements Parcelable {
    private String mName;
    private Object mMsgPushInfo;
    private PoiInfoEntity mPoiInfoEntity;
    private RoutePoint mStartPoint;
    private RoutePoint mEndPoint;
    private ArrayList<RoutePoint> mViaPoints = new ArrayList<>();
    private ArrayList<PoiInfoEntity> mViaPoiInfoEntity = new ArrayList<>();

    protected RouteMsgPushInfo(Parcel in) {
        mName = in.readString();
        mPoiInfoEntity = in.readParcelable(PoiInfoEntity.class.getClassLoader());
        mStartPoint = in.readParcelable(RoutePoint.class.getClassLoader());
        mEndPoint = in.readParcelable(RoutePoint.class.getClassLoader());
        mViaPoints = in.createTypedArrayList(RoutePoint.CREATOR);
        mViaPoiInfoEntity = in.createTypedArrayList(PoiInfoEntity.CREATOR);
    }

    public static final Creator<RouteMsgPushInfo> CREATOR = new Creator<RouteMsgPushInfo>() {
        @Override
        public RouteMsgPushInfo createFromParcel(Parcel in) {
            return new RouteMsgPushInfo(in);
        }

        @Override
        public RouteMsgPushInfo[] newArray(int size) {
            return new RouteMsgPushInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(mName);
        dest.writeParcelable(mPoiInfoEntity, flags);
        dest.writeParcelable(mStartPoint, flags);
        dest.writeParcelable(mEndPoint, flags);
        dest.writeTypedList(mViaPoints);
        dest.writeTypedList(mViaPoiInfoEntity);
    }
}
