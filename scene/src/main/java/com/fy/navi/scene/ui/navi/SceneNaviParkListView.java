package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.databinding.SceneNaviParkListViewBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.SceneNaviParkListImpl;
import com.fy.navi.scene.impl.navi.TimerHelper;
import com.fy.navi.scene.ui.adapter.NaviParkListAdapter;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviParkingEntity;

import java.util.List;

/**
 * 列表scene 停车场列表 (点击终点停车场悬挂卡片的时候显示)
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviParkListView extends NaviSceneBase<SceneNaviParkListViewBinding, SceneNaviParkListImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private NaviParkListAdapter mNaviParkListAdapter;

    public SceneNaviParkListView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviParkListView(@NonNull final Context context,
                                 @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviParkListView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                 final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_PARK_LIST;
    }

    @Override
    protected SceneNaviParkListViewBinding createViewBinding(final LayoutInflater inflater,
                                                             final ViewGroup viewGroup) {
        return SceneNaviParkListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviParkListImpl initSceneImpl() {
        return new SceneNaviParkListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviParkList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        Logger.d(TAG, "SceneNaviListView initObserver");
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.srvAddVia.setLayoutManager(layoutManager);
        mNaviParkListAdapter = new NaviParkListAdapter();
        mViewBinding.srvAddVia.setAdapter(mNaviParkListAdapter);
        // 滚动列表的时候刷新定时器
        mViewBinding.srvAddVia.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
                super.onScrolled(recyclerView, dx, dy);
                // 加入防暴力操作
                if (null != mScreenViewModel && TimerHelper.isCanDo()) {
                    startCountdown();
                    ImmersiveStatusScene.getInstance().setImmersiveStatus(
                            MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
                }
            }
        });
        mNaviParkListAdapter.setOnItemClickListener(mScreenViewModel);
    }

    public void updateData(List<NaviParkingEntity> list, final MapType mapType) {
        setScreenId(mapType);
        Logger.i(TAG, "updateData-size:" + list.size());
        if (ConvertUtils.isEmpty(list)) return;
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mScreenViewModel)) {
                mNaviParkListAdapter.notifyList(list, 0);
                notifySceneStateChange(true);
            }
        });
    }

    @Override
    public void show() {
        super.show();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.showPreview(mNaviParkListAdapter.getSelectIndex());
        }
    }

    @Override
    public void close() {
        super.close();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.exitPreview();
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.exitPreview();
        }
    }

    public void onItemClick(int index) {
        if (!ConvertUtils.isNull(mNaviParkListAdapter)) {
            mNaviParkListAdapter.notifyItemSelect(index);
        }
    }

    @Override
    public boolean isNeedAutoStartTimer() {
        return true;
    }
}
