package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.search.PoiInfoEntity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RouteSupplementInfo implements Parcelable {
    //补能点类别
    private int mType;
    //补能点距起点位置
    private int mDistance;
    //补能点间隔
    private int mInterval;
    //补能点距起点位置
    private String mUnitDistance;
    //poiID
    private String mPoiID;
    //补能点名称
    private String mName;
    //预计充电事件
    private int mChargeTime;
    //经纬度
    private Coord2DDouble mShow;
    //补能点信息
    private RouteChargeStationDetailInfo mRouteChargeStationDetailInfo;
    //替换补能点信息
    private PoiInfoEntity mPoiInfoEntity;
    //补能点顺序
    private int mSupplementIndex = -1;
    //该补能点是途经点时对应途经点的index
    private int mViaIndex = -1;


    protected RouteSupplementInfo(Parcel in) {
        mType = in.readInt();
        mDistance = in.readInt();
        mInterval = in.readInt();
        mUnitDistance = in.readString();
        mPoiID = in.readString();
        mName = in.readString();
        mChargeTime = in.readInt();
        mRouteChargeStationDetailInfo = in.readParcelable(RouteChargeStationDetailInfo.class.getClassLoader());
        mPoiInfoEntity = in.readParcelable(PoiInfoEntity.class.getClassLoader());
    }

    public static final Creator<RouteSupplementInfo> CREATOR = new Creator<RouteSupplementInfo>() {
        @Override
        public RouteSupplementInfo createFromParcel(Parcel in) {
            return new RouteSupplementInfo(in);
        }

        @Override
        public RouteSupplementInfo[] newArray(int size) {
            return new RouteSupplementInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(mType);
        dest.writeInt(mDistance);
        dest.writeInt(mInterval);
        dest.writeString(mUnitDistance);
        dest.writeString(mPoiID);
        dest.writeString(mName);
        dest.writeInt(mChargeTime);
        dest.writeParcelable(mRouteChargeStationDetailInfo, flags);
        dest.writeParcelable(mPoiInfoEntity, flags);
    }
}
