package com.fy.navi.hmi.poi;

import static com.android.utils.TimeUtils.isCurrentTimeInRange;

import android.app.Application;
import android.util.Log;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.CostTime;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
public class BasePoiDetailsViewModel extends BaseViewModel<PoiDetailsFragment, PoiDetailsModel> {
    public BasePoiDetailsViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected PoiDetailsModel initModel() {
        return new PoiDetailsModel();
    }

    /**
     * 搜索结果回调
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void onSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mView.onSearchResult(taskId, searchResultEntity);
    }

    /**
     * 搜索图层子点点击事件
     * @param index 点击下标
     */
    public void onMarkChildClickCallBack(final int index) {
        mView.onMarkChildClickCallBack(index);
    }

    public Action getRootClick() {
        return mRootClick;
    }

    private final Action mRootClick = () -> {
    };

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }

    /**
     * 判断是否在自车位
     * @return 是否在自车位
     */
    public boolean calcDistanceBetweenPoints(){
        return mModel.calcStraightDistance();
    }


    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        mModel.onReStoreFragment();
    }

    public void notifyNetSearchResult(int taskId,BaseRep result){
        if(!ConvertUtils.isNull(result)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "code" + result.getResultCode());
            JSONObject jsonObject = null;
            try {
                jsonObject = new JSONObject(String.valueOf(result.getDataSet()));
                // 处理经纬度
                GeoPoint point = new GeoPoint();
                point.setLat(ConvertUtils.str2Double(jsonObject.getString("stationLat")));
                point.setLon(ConvertUtils.str2Double(jsonObject.getString("stationLng")));
                JSONArray jsonArray = jsonObject.getJSONArray("pictures");
                ArrayList<ChargeInfo> chargeList = new ArrayList<>();
                ArrayList<PoiInfoEntity> poiInfoEntityList = new ArrayList<>();
                ChargeInfo chargeInfo = GsonUtils.fromJson(String.valueOf(result.getDataSet()),ChargeInfo.class);
                CostTime currentTime = getCurrentElePrice(chargeInfo.getCostItem());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"currentTime: "+currentTime.getTime());
                chargeInfo.setCurrentElePrice(currentTime.getmElectricityFee());
                chargeList.add(chargeInfo);
                PoiInfoEntity poiInfoEntity = GsonUtils.fromJson(String.valueOf(result.getDataSet()),PoiInfoEntity.class);
                poiInfoEntity.setChargeInfoList(chargeList)
                        .setImageUrl(jsonArray.getString(0))
                        .setPoint(point)
                        .setPointTypeCode("011100");
                poiInfoEntityList.add(poiInfoEntity);
                SearchResultEntity searchResultEntity = new SearchResultEntity()
                        .setIsNetData(true)
                        .setPoiList(poiInfoEntityList)
                        .setPoiType(1);
                final ThreadManager threadManager = ThreadManager.getInstance();
                threadManager.postUi(() -> {
                    mView.onSearchResult(taskId,searchResultEntity);
                });
            } catch (JSONException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"JSONException: "+e);
            }
        }
    }

    private CostTime getCurrentElePrice(ArrayList<CostTime> costTimes) {
        CostTime currentCostTime = new CostTime();
        if (!ConvertUtils.isEmpty(costTimes)) {
            for (int i = 0; i < costTimes.size(); i++) {
                if (isCurrentTimeInRange(costTimes.get(i).getTime())) {
                    currentCostTime = costTimes.get(i);
                }
            }
        }
        return currentCostTime;
    }

}
