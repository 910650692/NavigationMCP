package com.sgm.navi.scene.impl.favorite;


import android.text.TextUtils;


import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.favorite.ISceneMapPointSearchView;
import com.sgm.navi.scene.ui.favorite.SceneMapPointSearchView;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.StackManager;


/**
 * @author qlzou
 * @version \$Revision1.0\$
 * @Description: 地图选点
 * @CreateDate: $ $
 */
public class SceneMapPointSearchViewImpl extends BaseSceneModel<SceneMapPointSearchView> implements ISceneMapPointSearchView {
    private final SearchPackage mSearchPackage;
    private final MapPackage mMapPackage;

    private final LayerPackage mLayerPackage;
    private int mCommonName;
    private int mTaskId;

    /**
     * 获取高德SDK请求任务Id
     * @return 请求任务Id
     */
    public int getMTaskId() {
        return mTaskId;
    }

    public SceneMapPointSearchViewImpl(final SceneMapPointSearchView scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
    }

    @Override
    public void closeFragment() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mSearchPackage.clearLabelMark();
        flyLineVisible(false);
        if (null != mScreenView) {
            mScreenView.closeMapPointView();
        }
    }

    public void setCommonName(final int commonName) {
        mCommonName = commonName;
    }

    /**
     * 关闭所有的fragment
     */
    public void closeAllFragment(){
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeAllFragment();
    }

    @Override
    public void clickSetting() {
        mScreenView.clickSetting();
        mScreenView.closeMapPointView();
        flyLineVisible(false);
    }

    /**
     * 清除地图选点POI扎标
     */
    public void clearPoiLabelMark() {
        mSearchPackage.clearPoiLabelMark();
    }

    /**
     * 执行搜索方法
     *
     * @param poiInfoEntity poiInfoEntity
     */
    public void doSearch(final PoiInfoEntity poiInfoEntity) {
        if (null == poiInfoEntity) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "doSearch: currentLocationSearch");
            mTaskId = mSearchPackage.currentLocationSearch();
            mMapPackage.goToCarPosition(MapType.MAIN_SCREEN_MAIN_MAP, true, true);
            return;
        }
        // 这里只有两种搜索类型：POI搜索和Geo搜索
        if (!TextUtils.isEmpty(poiInfoEntity.getPid())) {
            mTaskId = mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
        } else {
            mTaskId = mSearchPackage.geoSearch(poiInfoEntity.getPoint());
        }
    }


    /**
     * 隐藏/显示移图选点
     * @param visible   是否显示
     */
    public void flyLineVisible(final boolean visible){
        mLayerPackage.openFlyLine(mMapTypeId, visible);
    }
}
