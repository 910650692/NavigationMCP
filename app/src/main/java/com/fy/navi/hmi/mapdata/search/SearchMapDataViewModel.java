package com.fy.navi.hmi.mapdata.search;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

public class SearchMapDataViewModel extends BaseViewModel<SearchMapDataFragment, SearchMapDataModel> {
    private static final String TAG = SearchMapDataModel.class.getSimpleName();

    public SearchMapDataViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SearchMapDataModel initModel() {
        return new SearchMapDataModel();
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param keyword
     */
    public void searchAdCode(final String keyword) {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                Logger.d(TAG, "sugSearch search: " + ", Keyword: " + keyword);
                mView.updateSearchResultView(mModel.searchAdCode(keyword));
            }
        }, 0);
    }

    /**
     * 返回上一页
     */
    public Action mCloseSearchMapDataView = () -> {
        closeFragment(true);
        // mScreenView.clearEditText();
    };

    /**
     * 清除搜索框内容
     */
    public Action mClearEdit = () -> {
        mView.clearEditText();
    };

    /**
     * @param adCodeList 省份、城市ID列表
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mModel.startAllTask(adCodeList);
    }

    /**
     * 暂停正在下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

    /**
     * 取消下载
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mModel.cancelAllTask(adCodeList);
    }

    /**
     * 更新数据下载状态
     * @param cityDataInfo
     */
    public void onDownLoadStatus(final CityDataInfo cityDataInfo) {
        mView.notifySearchMapDataChangeView(cityDataInfo);
    }

}
