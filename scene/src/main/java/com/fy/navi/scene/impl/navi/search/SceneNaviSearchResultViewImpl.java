package com.fy.navi.scene.impl.navi.search;

import androidx.databinding.ObservableField;

import com.android.utils.ResourceUtils;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.ui.navi.search.SceneNaviSearchResultView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviSearchResultViewImpl extends BaseSceneModel<SceneNaviSearchResultView> {

    public static final String TAG = "SceneNaviSearchResultViewImpl";

    public ObservableField<String> mKeyWordField;

    // 0显示充电站列表，1显示其他列表
    public ObservableField<Integer> mResultVisibleType;
    public SceneNaviSearchResultViewImpl(final SceneNaviSearchResultView mScreenView) {
        super(mScreenView);
        mKeyWordField = new ObservableField<>("");
        mResultVisibleType = new ObservableField<>(0);
    }

    public void updateData(final SearchResultEntity searchResultEntity) {
        if (null != searchResultEntity) {
            String keyword = searchResultEntity.getKeyword();
            mKeyWordField.set(keyword);
            if (ResourceUtils.Companion.getInstance().getString(R.string.st_quick_search_charge).
                    equals(keyword)) {
                mResultVisibleType.set(0);
                List<RouteParam> allPoiParamList = new ArrayList<>();
                allPoiParamList = OpenApiHelper.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
                if (allPoiParamList.size() >= 2) {
                    allPoiParamList.remove(0);
                    allPoiParamList.remove(allPoiParamList.size() - 1);
                }
                mScreenView.showNaviSearchChargeListUI(searchResultEntity.getPoiList(),
                        allPoiParamList, OpenApiHelper.getSearchType());
            } else {
                mResultVisibleType.set(1);
                List<PoiInfoEntity> poiInfoEntityList = new ArrayList<>();
                poiInfoEntityList = searchResultEntity.getPoiList();
                mScreenView.showNaviOtherListUI(poiInfoEntityList);
            }
        }
    }

    public void closeSearchResultView() {
        mScreenView.closeSearchResultView();
    }

    /**
     * @param type 0:沿途搜 1:终点搜 2:周边搜
     */
    public void onSearTypeClick(int type) {
        mScreenView.onSearTypeClick(mKeyWordField.get(), type);
    }

}
