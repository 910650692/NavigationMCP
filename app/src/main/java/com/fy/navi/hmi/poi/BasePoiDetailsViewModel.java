package com.fy.navi.hmi.poi;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

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

}
