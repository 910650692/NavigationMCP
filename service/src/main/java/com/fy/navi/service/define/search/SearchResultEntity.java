package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.AutoMapConstant;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;


/**
 * @author baipeng0904
 * @version \$Revision1.0\$
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
    private int mSearchType;
    // 搜索关键字
    private String mKeyword;
    // 搜索返回的最大页数
    private int maxPageNum = 1;
    // 搜索当前页数
    private int mPageNum = 1;
    // GBL 返回对应的错误码
    @SearchErrorCode.ErrorCode
    private int mCode;
    // 错误信息
    private String message;
    // poi 列表
    private List<PoiInfoEntity> mPoiList;
    private float mZoomLeve;
    private String mRetain;
    private List<SearchCategoryLocalInfo> mLocalInfoList;
    private List<SearchCategoryLocalInfo> mLevel2LocalInfoList;
    //返回的总item数
    private int mTotal;
    private int mPoiType;
    private ArrayList<String> mQueryTypeList;
    private boolean mIsNetData = false;

    public int getSearchType() {
        return mSearchType;
    }

    /**
     * HMI 单独定义的搜索类型和gbl返回的无关联，用于处理自己单独的业务
     * @param searchType 搜索类型
     * @return SearchResultEntity
     */
    public SearchResultEntity setSearchType(final int searchType) {
        this.mSearchType = searchType;
        return this;
    }

    public String getKeyword() {
        return mKeyword;
    }

    /**
     * 搜索关键字
     * @param keyword 关键字
     * @return SearchResultEntity
     */
    public SearchResultEntity setKeyword(final String keyword) {
        this.mKeyword = keyword;
        return this;
    }

    public int getMaxPageNum() {
        return maxPageNum;
    }

    /**
     * 搜索返回的最大页数
     * @param maxPageNum 最大页数
     * @return SearchResultEntity
     */
    public SearchResultEntity setMaxPageNum(final int maxPageNum) {
        this.maxPageNum = maxPageNum;
        return this;
    }

    public int getPageNum() {
        return mPageNum;
    }

    /**
     * 搜索当前页数
     * @param pageNum 页数
     * @return SearchResultEntity
     */
    public SearchResultEntity setPageNum(final int pageNum) {
        this.mPageNum = pageNum;
        return this;
    }

    public int getCode() {
        return mCode;
    }

    /**
     * 返回的状态码
     * @param code 状态码
     * @return SearchResultEntity
     */
    public SearchResultEntity setCode(final int code) {
        this.mCode = code;
        return this;
    }

    public String getMessage() {
        return message;
    }

    /**
     * 返回的状态信息
     * @param message 状态信息
     * @return SearchResultEntity
     */
    public SearchResultEntity setMessage(final String message) {
        this.message = message;
        return this;
    }

    public List<PoiInfoEntity> getPoiList() {
        return mPoiList;
    }

    /**
     * 搜索结果的poi列表
     * @param poiList poi列表
     * @return SearchResultEntity
     */
    public SearchResultEntity setPoiList(final List<PoiInfoEntity> poiList) {
        this.mPoiList = poiList;
        return this;
    }

    public float getZoomLeve() {
        return mZoomLeve;
    }

    /**
     * 地图缩放级别
     * @param zoomLeve 缩放级别
     * @return SearchResultEntity
     */
    public SearchResultEntity setZoomLeve(final float zoomLeve) {
        this.mZoomLeve = zoomLeve;
        return this;
    }

    public String getRetain() {
        return mRetain;
    }

    /**
     * 搜索回传参数，用于筛选搜索时传参
     * @param retain 回传参数
     * @return SearchResultEntity
     */
    public SearchResultEntity setRetain(final String retain) {
        this.mRetain = retain;
        return this;
    }

    public List<SearchCategoryLocalInfo> getLocalInfoList() {
        return mLocalInfoList;
    }

    /**
     * 设置筛选列表
     * @param localInfoList 筛选列表
     * @return SearchResultEntity
     */
    public SearchResultEntity setLocalInfoList(final List<SearchCategoryLocalInfo> localInfoList) {
        this.mLocalInfoList = localInfoList;
        return this;
    }

    public int getTotal() {
        return mTotal;
    }

    /**
     * 搜索结果的总数量
     * @param total 总数量
     * @return SearchResultEntity
     */
    public SearchResultEntity setTotal(final int total) {
        this.mTotal = total;
        return this;
    }

    public int getPoiType() {
        return mPoiType;
    }

    /**
     * 设置poi类型
     * @param poiType poi类型
     * @return SearchResultEntity
     */
    public SearchResultEntity setPoiType(final int poiType) {
        this.mPoiType = poiType;
        return this;
    }

    public ArrayList<String> getQueryTypeList() {
        return mQueryTypeList;
    }

    // 设置用户查询意图
    public SearchResultEntity setQueryTypeList(final ArrayList<String> list){
        this.mQueryTypeList = list;
        return this;
    }

    public boolean getIsNetData(){
        return mIsNetData;
    }

    public SearchResultEntity setIsNetData(final boolean isNetData){
        this.mIsNetData = isNetData;
        return this;
    }

    protected SearchResultEntity(final Parcel in) {
        mSearchType = in.readInt();
        mKeyword = in.readString();
        maxPageNum = in.readInt();
        mPageNum = in.readInt();
        mCode = in.readInt();
        message = in.readString();
        mPoiList = in.createTypedArrayList(PoiInfoEntity.CREATOR);
        mZoomLeve = in.readFloat();
        mRetain = in.readString();
        mLocalInfoList = in.createTypedArrayList(SearchCategoryLocalInfo.CREATOR);
        mLevel2LocalInfoList = in.createTypedArrayList(SearchCategoryLocalInfo.CREATOR);
        mTotal = in.readInt();
        mPoiType = in.readInt();
        mQueryTypeList = in.createStringArrayList();
        mIsNetData = in.readBoolean();
    }

    public static final Creator<SearchResultEntity> CREATOR = new Creator<SearchResultEntity>() {
        @Override
        public SearchResultEntity createFromParcel(final Parcel in) {
            return new SearchResultEntity(in);
        }

        @Override
        public SearchResultEntity[] newArray(final int size) {
            return new SearchResultEntity[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel dest, final int flags) {
        dest.writeInt(mSearchType);
        dest.writeString(mKeyword);
        dest.writeInt(maxPageNum);
        dest.writeInt(mPageNum);
        dest.writeInt(mCode);
        dest.writeString(message);
        dest.writeTypedList(mPoiList);
        dest.writeFloat(mZoomLeve);
        dest.writeString(mRetain);
        dest.writeTypedList(mLocalInfoList);
        dest.writeTypedList(mLevel2LocalInfoList);
        dest.writeInt(mTotal);
        dest.writeInt(mPoiType);
        dest.writeStringList(mQueryTypeList);
        dest.writeBoolean(mIsNetData);
    }
}
