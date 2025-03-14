package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.ArrayList;

public class BaseRouteResult implements Parcelable {

    private long requestId; //请求Id
    private int mapId; //算路结果对应的Map
    private boolean fastNavi; //是否快速导航
    private boolean isOnlineRoute;
    private ArrayList<BaseRouteLine> routeLineInfos;

    public BaseRouteResult() {}

    public BaseRouteResult(Parcel in) {
        requestId = in.readLong();
        mapId = in.readInt();
        fastNavi = in.readByte() == 1;
        isOnlineRoute = in.readByte() == 1;
        routeLineInfos = in.createTypedArrayList(BaseRouteLine.CREATOR);
    }

    public static final Creator<BaseRouteResult> CREATOR = new Creator<BaseRouteResult>() {
        @Override
        public BaseRouteResult createFromParcel(Parcel source) {
            return new BaseRouteResult(source);
        }

        @Override
        public BaseRouteResult[] newArray(int size) {
            return new BaseRouteResult[size];
        }
    };

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeLong(requestId);
        dest.writeInt(mapId);
        dest.writeByte((byte) (fastNavi ? 1 : 0));
        dest.writeByte((byte) (isOnlineRoute ? 1 : 0));
        dest.writeTypedList(routeLineInfos);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public long getRequestId() {
        return requestId;
    }

    public void setRequestId(long requestId) {
        this.requestId = requestId;
    }

    public int getMapId() {
        return mapId;
    }

    public void setMapId(int mapId) {
        this.mapId = mapId;
    }

    public boolean isFastNavi() {
        return fastNavi;
    }

    public void setFastNavi(boolean fastNavi) {
        this.fastNavi = fastNavi;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public ArrayList<BaseRouteLine> getRouteLineInfos() {
        return routeLineInfos;
    }

    public void setRouteLineInfos(ArrayList<BaseRouteLine> setRouteLineInfos) {
        this.routeLineInfos = setRouteLineInfos;
    }

}
