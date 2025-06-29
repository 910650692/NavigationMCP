package com.sgm.navi.service.define.route;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.List;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RouteSpeechRequestParam implements Parcelable {
    private MapType mMapTypeId; //必要参数
    private PoiInfoEntity mStartPoiInfoEntity;
    private PoiInfoEntity mEndPoiInfoEntity;//必要参数
    private List<PoiInfoEntity> mViaPoiInfoEntityList;
    private RoutePreferenceID mPreferenceID;

    protected RouteSpeechRequestParam(Parcel in) {
        mStartPoiInfoEntity = in.readParcelable(PoiInfoEntity.class.getClassLoader());
        mEndPoiInfoEntity = in.readParcelable(PoiInfoEntity.class.getClassLoader());
        mViaPoiInfoEntityList = in.createTypedArrayList(PoiInfoEntity.CREATOR);
    }

    public static final Creator<RouteSpeechRequestParam> CREATOR = new Creator<RouteSpeechRequestParam>() {
        @Override
        public RouteSpeechRequestParam createFromParcel(Parcel in) {
            return new RouteSpeechRequestParam(in);
        }

        @Override
        public RouteSpeechRequestParam[] newArray(int size) {
            return new RouteSpeechRequestParam[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeParcelable(mStartPoiInfoEntity, flags);
        dest.writeParcelable(mEndPoiInfoEntity, flags);
        dest.writeTypedList(mViaPoiInfoEntityList);
    }
}