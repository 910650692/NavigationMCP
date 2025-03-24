package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.ArrayList;

public class BaseRouteResult implements Parcelable {

    private long mRequestId; //请求Id
    private int mMapId; //算路结果对应的Map
    private boolean mFastNavi; //是否快速导航
    private boolean mIsOnlineRoute;
    private ArrayList<BaseRouteLine> mRouteLineInfos;

    public BaseRouteResult() {
        mRouteLineInfos = new ArrayList<>();
    }

    public BaseRouteResult(final Parcel in) {
        mRequestId = in.readLong();
        mMapId = in.readInt();
        mFastNavi = in.readByte() == 1;
        mIsOnlineRoute = in.readByte() == 1;
        mRouteLineInfos = in.createTypedArrayList(BaseRouteLine.CREATOR);
    }

    public static final Creator<BaseRouteResult> CREATOR = new Creator<BaseRouteResult>() {
        @Override
        public BaseRouteResult createFromParcel(final Parcel source) {
            return new BaseRouteResult(source);
        }

        @Override
        public BaseRouteResult[] newArray(final int size) {
            return new BaseRouteResult[size];
        }
    };

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeLong(mRequestId);
        dest.writeInt(mMapId);
        dest.writeByte((byte) (mFastNavi ? 1 : 0));
        dest.writeByte((byte) (mIsOnlineRoute ? 1 : 0));
        dest.writeTypedList(mRouteLineInfos);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public long getRequestId() {
        return mRequestId;
    }

    public void setRequestId(final long requestId) {
        mRequestId = requestId;
    }

    public int getMapId() {
        return mMapId;
    }

    public void setMapId(final int mapId) {
        mMapId = mapId;
    }

    public boolean isFastNavi() {
        return mFastNavi;
    }

    public void setFastNavi(final boolean fastNavi) {
        mFastNavi = fastNavi;
    }

    public boolean isOnlineRoute() {
        return mIsOnlineRoute;
    }

    public void setOnlineRoute(final boolean onlineRoute) {
        mIsOnlineRoute = onlineRoute;
    }

    public ArrayList<BaseRouteLine> getRouteLineInfos() {
        return mRouteLineInfos;
    }

    public void setRouteLineInfos(final ArrayList<BaseRouteLine> routeLineInfos) {
        mRouteLineInfos = routeLineInfos;
    }

}
