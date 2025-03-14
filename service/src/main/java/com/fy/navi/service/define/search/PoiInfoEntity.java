package com.fy.navi.service.define.search;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.autonavi.gbl.search.model.SearchRetainParam;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class PoiInfoEntity implements Parcelable {
    private int poiType;           // poi 类型
    private String pid;            // 父POI的Id
    private String name;           // 名称
    private String phone;          // 电话
    private String address;        // 地址
    private String category;       // 分类，筛选使用
    private int adCode;            // 城市代码
    private String distance;       // 距离，默认带单位
    private String typeCode;       // POI搜索类型
    private String poiTag;         // POI标签
    private String shortName;      // 简称
    private String businessTime;    // 营业时间
    private double ratio;          // 选择率
    private String openStatus;     // 营业状态
    private int hisMark;           // 个性化标签（美食、洗车）
    private String scenicMark;     // 个性化标签（景区）
    private int averageCost;       // 人均消费
    private String rating;         // 星级评分
    private String imageUrl;       // 图片信息
    private GeoPoint point;        // 经纬度
    private String pointTypeCode;  // POI类型（扎标专用）
    private String industry;       // 行业类型

    // -----------语音排序使用------------
    private int sort_distance;     // 距离，语音使用
    private int sort_rate;        // 评价，语音使用
    private int sort_price;      // 价格，语音使用

    //-----------
    private CityInfo cityInfo;
    // 收藏相关
    private FavoriteInfo favoriteInfo;
    //回传参数，POI详情搜索必传，来自于关键字搜索结果
    private SearchRetainParamInfo retainParam;
    // 服务列表区信息
    private List<ServiceAreaInfo> serviceAreaInfoList;
    // 停车场列表信息
    private List<ParkingInfo> parkingInfoList;
    // 充电列表站信息
    private List<ChargeInfo> chargeInfoList;
    // 加油站列表信息
    private List<GasStationInfo> stationList;
    // 子POI列表信息
    private List<ChildInfo> childInfoList;

    protected PoiInfoEntity(Parcel in) {
        poiType = in.readInt();
        pid = in.readString();
        name = in.readString();
        phone = in.readString();
        address = in.readString();
        category = in.readString();
        adCode = in.readInt();
        distance = in.readString();
        typeCode = in.readString();
        poiTag = in.readString();
        shortName = in.readString();
        businessTime = in.readString();
        ratio = in.readDouble();
        openStatus = in.readString();
        hisMark = in.readInt();
        scenicMark = in.readString();
        averageCost = in.readInt();
        rating = in.readString();
        imageUrl = in.readString();
        point = in.readParcelable(GeoPoint.class.getClassLoader());
        pointTypeCode = in.readString();
        industry = in.readString();
        sort_distance = in.readInt();
        sort_rate = in.readInt();
        sort_price = in.readInt();
        cityInfo = in.readParcelable(CityInfo.class.getClassLoader());
        favoriteInfo = in.readParcelable(FavoriteInfo.class.getClassLoader());
        retainParam = in.readParcelable(SearchRetainParamInfo.class.getClassLoader());
        serviceAreaInfoList = in.createTypedArrayList(ServiceAreaInfo.CREATOR);
        parkingInfoList = in.createTypedArrayList(ParkingInfo.CREATOR);
        chargeInfoList = in.createTypedArrayList(ChargeInfo.CREATOR);
        stationList = in.createTypedArrayList(GasStationInfo.CREATOR);
        childInfoList = in.createTypedArrayList(ChildInfo.CREATOR);
    }

    public static final Creator<PoiInfoEntity> CREATOR = new Creator<PoiInfoEntity>() {
        @Override
        public PoiInfoEntity createFromParcel(Parcel in) {
            return new PoiInfoEntity(in);
        }

        @Override
        public PoiInfoEntity[] newArray(int size) {
            return new PoiInfoEntity[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(poiType);
        parcel.writeString(pid);
        parcel.writeString(name);
        parcel.writeString(phone);
        parcel.writeString(address);
        parcel.writeString(category);
        parcel.writeInt(adCode);
        parcel.writeString(distance);
        parcel.writeString(typeCode);
        parcel.writeString(poiTag);
        parcel.writeString(shortName);
        parcel.writeString(businessTime);
        parcel.writeDouble(ratio);
        parcel.writeString(openStatus);
        parcel.writeInt(hisMark);
        parcel.writeString(scenicMark);
        parcel.writeInt(averageCost);
        parcel.writeString(rating);
        parcel.writeString(imageUrl);
        parcel.writeParcelable(point, i);
        parcel.writeString(pointTypeCode);
        parcel.writeString(industry);
        parcel.writeInt(sort_distance);
        parcel.writeInt(sort_rate);
        parcel.writeInt(sort_price);
        parcel.writeParcelable(cityInfo, i);
        parcel.writeParcelable(favoriteInfo, i);
        parcel.writeParcelable(retainParam, i);
        parcel.writeTypedList(serviceAreaInfoList);
        parcel.writeTypedList(parkingInfoList);
        parcel.writeTypedList(chargeInfoList);
        parcel.writeTypedList(stationList);
        parcel.writeTypedList(childInfoList);
    }
}