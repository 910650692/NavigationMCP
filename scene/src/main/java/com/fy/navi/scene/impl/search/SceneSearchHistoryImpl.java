package com.fy.navi.scene.impl.search;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchHistory;
import com.fy.navi.scene.ui.search.SceneMainSearchBottomPartView;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import java.util.ArrayList;
import java.util.List;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索历史记录
 * @CreateDate: $ $
 */
public class SceneSearchHistoryImpl extends BaseSceneModel<SceneMainSearchBottomPartView> implements ISceneSearchHistory {
    private final SearchPackage mSearchPackage;
    private final BehaviorPackage mBehaviorPackage;

    public SceneSearchHistoryImpl(final SceneMainSearchBottomPartView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
        mBehaviorPackage = BehaviorPackage.getInstance();
    }

    /**
     * 常用地址选择
     * @param position 点击下标
     */
    public void commonAdd(final int position){
        //判断是否有常用地址
        //有常用地址打开详情页
        final ArrayList<PoiInfoEntity> list = mBehaviorPackage.getFavoriteAddressInfo();
        final int size = list.size();
        if (size > 0 && position < size) {
            mScreenView.jumpToPoiFragment(list.get(position));
        } else {
            //没有就跳转到选择页
            mScreenView.jumpToHomeCompanyFragment();
        }
    }




    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void getSearchKeywordRecord() {
        final List<History> historyList = mSearchPackage.getSearchKeywordRecord();
        mScreenView.notifyKeywordRecord(historyList);
    }

    @Override
    public void clearSearchKeywordRecord() {
        mSearchPackage.clearSearchKeywordRecord();
    }
}
