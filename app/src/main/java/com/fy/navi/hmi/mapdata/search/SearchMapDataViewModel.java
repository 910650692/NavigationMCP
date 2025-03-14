package com.fy.navi.hmi.mapdata.search;

import static com.fy.navi.service.MapDefaultFinalTag.OFFLINE_HMI_TAG;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/09
 */
public class SearchMapDataViewModel extends BaseViewModel<SearchMapDataFragment, SearchMapDataModel> {

    public SearchMapDataViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SearchMapDataModel initModel() {
        return new SearchMapDataModel();
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     */
    public void searchAdCode(String keyword) {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                Logger.d(OFFLINE_HMI_TAG, "sugSearch search: " + ", Keyword: " + keyword);
                mView.updateSearchResultView(mModel.searchAdCode(keyword));
            }
        }, 0);
    }

    /**
     * 返回上一页
     */
    public Action closeSearchMapDataView = () -> {
        closeFragment(true);
        // mScreenView.clearEditText();
    };

    /**
     * 清除搜索框内容
     */
    public Action clearEdit = () -> {
        mView.clearEditText();
    };

    /**
     * @param adCodeList 省份、城市ID列表
     */
    public void startAllTask(ArrayList<Integer> adCodeList) {
        mModel.startAllTask(adCodeList);
    }

    /**
     * 暂停正在下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void pauseAllTask(ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

    public void cancelAllTask(ArrayList<Integer> adCodeList) {
        mModel.cancelAllTask(adCodeList);
    }

    public void onPercent(ProvDataInfo info) {

    }

}
