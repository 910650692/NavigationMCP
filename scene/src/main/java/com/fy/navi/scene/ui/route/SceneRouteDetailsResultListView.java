package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import com.android.utils.ConvertUtils;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRouteDetailsSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteDetailsResultListViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteDetailsResultListImpl;
import com.fy.navi.scene.ui.adapter.RouteDetailsResultAdapter;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;


public class SceneRouteDetailsResultListView extends BaseSceneView<SceneRouteDetailsResultListViewBinding, SceneRouteDetailsResultListImpl> {

    private RouteDetailsResultAdapter mRouteDetailsResultAdapter;
    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos;
    private boolean mIsAvoid = false;
    private RouteAvoidInfo mRouteAvoidInfo;
    private Hashtable<String, ISceneRouteDetailsSelectCallBack> mSceneRouteDetailsSelectCallBackHashtable;

    public SceneRouteDetailsResultListView(final Context context) {
        super(context);
    }

    public SceneRouteDetailsResultListView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteDetailsResultListView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteDetailsResultListViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteDetailsResultListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteDetailsResultListImpl initSceneImpl() {
        mRouteLineSegmentInfos = new ArrayList<>();
        mSceneRouteDetailsSelectCallBackHashtable = new Hashtable<>();
        mRouteAvoidInfo = new RouteAvoidInfo();
        return new SceneRouteDetailsResultListImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteDeatailsCheckedObserver(final String key, final ISceneRouteDetailsSelectCallBack callBack) {
        mSceneRouteDetailsSelectCallBackHashtable.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    public SceneRouteDetailsResultListViewBinding getBinding() {
        return mViewBinding;
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
    }
    /**
     * 初始化列表
     * */
    private void setupRecyclerView() {
        mRouteDetailsResultAdapter = new RouteDetailsResultAdapter();
        mRouteDetailsResultAdapter.setOnItemCheckedChangeListener(routeAvoidInfo -> {
            mRouteAvoidInfo = routeAvoidInfo;
            for (ISceneRouteDetailsSelectCallBack callBack : mSceneRouteDetailsSelectCallBackHashtable.values()) {
                callBack.onRouteDetailsChecked(routeAvoidInfo.isMCheckedLeastOne());
            }
        });
        mViewBinding.routeDetailResultList.setAdapter(mRouteDetailsResultAdapter);
    }
    /**
     * 初始化列表
     * @param routeLineSegmentInfos 列表数据
     * */
    public void notifyRouteDetailsResultList(final List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        if (ConvertUtils.isEmpty(routeLineSegmentInfos)) {
            return;
        }
        mRouteLineSegmentInfos = routeLineSegmentInfos;
        mRouteDetailsResultAdapter.setAdapterResult(routeLineSegmentInfos, mIsAvoid);
    }
    /**
     * 关闭列表
     * */
    private void closeAllItem() {
        if (mViewBinding.routeDetailResultList.getCount() <= NumberUtils.NUM_0) {
            return;
        }
        for (int t= NumberUtils.NUM_0; t < mViewBinding.routeDetailResultList.getCount(); t++) {
            if (mViewBinding.routeDetailResultList.isGroupExpanded(t)) {
                mViewBinding.routeDetailResultList.collapseGroup(t);
            }
        }
    }
    /**
     * 设置避开状态
     * @param isAvoid 避开状态
     * */
    public void setAvoidStatus(final boolean isAvoid) {
        this.mIsAvoid = isAvoid;
        if (ConvertUtils.isEmpty(mRouteLineSegmentInfos)) {
            return;
        }
        closeAllItem();
        mRouteDetailsResultAdapter.setAdapterResult(mRouteLineSegmentInfos, isAvoid);
    }
    /**
     * 设置终点名称
     * @param name 终点名称
     * */
    public void setEndPoint(final String name) {
        mViewBinding.routeDetailsListTvFooter.setText(AppContext.getInstance().getMContext().getResources().getString(R.string.route_details_list_footer) + name);
    }
    /**
     * 避开道路
     * */
    public void startAvoidRoad() {
        mScreenViewModel.startAvoidRoad(mRouteAvoidInfo);
    }
}
