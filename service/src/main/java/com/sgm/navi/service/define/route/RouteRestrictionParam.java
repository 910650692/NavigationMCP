package com.sgm.navi.service.define.route;

import com.sgm.navi.service.define.aos.RestrictedArea;
import com.sgm.navi.service.define.map.MapType;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RouteRestrictionParam implements Parcelable{
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    private boolean mIsOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteRestrictionInfo> mRouteRestrictionInfo = new ArrayList<>();

    private List<String> mRuleIds = new ArrayList<>();

    private Object mReStrictedAreaResponseParam;

    private RestrictedArea mRestrictedArea;

    protected RouteRestrictionParam(Parcel in) {
        mRequestId = in.readLong();
        mIsOnlineRoute = in.readByte() != 0;
        mRouteRestrictionInfo = in.createTypedArrayList(RouteRestrictionInfo.CREATOR);
        mRuleIds = in.createStringArrayList();
        mRestrictedArea = in.readParcelable(RestrictedArea.class.getClassLoader());
    }

    public static final Creator<RouteRestrictionParam> CREATOR = new Creator<RouteRestrictionParam>() {
        @Override
        public RouteRestrictionParam createFromParcel(Parcel in) {
            return new RouteRestrictionParam(in);
        }

        @Override
        public RouteRestrictionParam[] newArray(int size) {
            return new RouteRestrictionParam[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeLong(mRequestId);
        dest.writeByte((byte) (mIsOnlineRoute ? 1 : 0));
        dest.writeTypedList(mRouteRestrictionInfo);
        dest.writeStringList(mRuleIds);
        dest.writeParcelable(mRestrictedArea, flags);
    }
}
