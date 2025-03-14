package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.AutoMapConstant;

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
public class SearchResultEntity implements Parcelable {
    // HMI 单独定义的搜索类型和gbl返回的无关联，用于处理自己单独的业务
    @AutoMapConstant.SearchType
    private int searchType;
    // 搜索关键字
    private String keyword;
    // 搜索返回的最大页数
    private int maxPageNum = 1;
    // 搜索当前页数
    private int pageNum = 1;
    // GBL 返回对应的错误码
    @SearchErrorCode.ErrorCode
    private int code;
    // 错误信息
    private String message;
    // poi 列表
    private List<PoiInfoEntity> poiList;
    private float zoomLeve;
    private String retain;
    private List<SearchCategoryLocalInfo> localInfoList;
    //返回的总item数
    private int total;
    private int poiType;

    protected SearchResultEntity(Parcel in) {
        searchType = in.readInt();
        keyword = in.readString();
        maxPageNum = in.readInt();
        pageNum = in.readInt();
        code = in.readInt();
        message = in.readString();
        poiList = in.createTypedArrayList(PoiInfoEntity.CREATOR);
        zoomLeve = in.readFloat();
        retain = in.readString();
        localInfoList = in.createTypedArrayList(SearchCategoryLocalInfo.CREATOR);
        total = in.readInt();
        poiType = in.readInt();
    }

    public static final Creator<SearchResultEntity> CREATOR = new Creator<SearchResultEntity>() {
        @Override
        public SearchResultEntity createFromParcel(Parcel in) {
            return new SearchResultEntity(in);
        }

        @Override
        public SearchResultEntity[] newArray(int size) {
            return new SearchResultEntity[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(searchType);
        dest.writeString(keyword);
        dest.writeInt(maxPageNum);
        dest.writeInt(pageNum);
        dest.writeInt(code);
        dest.writeString(message);
        dest.writeTypedList(poiList);
        dest.writeFloat(zoomLeve);
        dest.writeString(retain);
        dest.writeTypedList(localInfoList);
        dest.writeInt(total);
        dest.writeInt(poiType);
    }
}
