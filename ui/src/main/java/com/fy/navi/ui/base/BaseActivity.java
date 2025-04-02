package com.fy.navi.ui.base;

import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public abstract class BaseActivity<V extends ViewDataBinding, VM extends BaseViewModel>
        extends AppCompatActivity implements IBaseView {
    protected V mBinding;
    protected VM mViewModel;
    protected StackManager mStackManager;
    protected String mScreenId;

    public BaseActivity() {
        super();
        Logger.i(getClass().getSimpleName(), "onCreate before");
        mStackManager = StackManager.getInstance();
        onCreateBefore();
        mStackManager.push(mScreenId, this);
    }

    @Override
    protected void onCreate(final @Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Logger.i(getClass().getSimpleName(), "onCreate start");
        setImmersiveStatusBar();
        createViewModel();
        onInitView();
        onInitObserver();
        onInitData();
        Logger.i(getClass().getSimpleName(), "onCreate end");
    }

    @Override
    protected void onResume() {
        super.onResume();
        Logger.i(getClass().getSimpleName(), "onResume");
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.i(getClass().getSimpleName(), "onStart");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Logger.i(getClass().getSimpleName(), "onStop");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.i(getClass().getSimpleName(), "onDestroy");
    }

    @Override
    public void finish() {
        super.finish();
        mStackManager.popActivity(mScreenId);
    }

    @Override
    public void addFragment(final BaseFragment fragment, final Bundle bundle) {
        FragmentIntent.addFragment(mScreenId, onFragmentId(), getSupportFragmentManager(),
                fragment, bundle);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter(bundle);
        }
    }

    @Override
    public void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle) {
        FragmentIntent.addPoiDetailsFragment(mScreenId, onFragmentId(), getSupportFragmentManager(),
                fragment, bundle);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
    }

    @Override
    public void closeFragment(final boolean nextShow) {
        FragmentIntent.closeFragment(mScreenId, getSupportFragmentManager(),
                mStackManager.getCurrentFragment(mScreenId), nextShow);
        Bundle bundle = null;
        Fragment fragment = mStackManager.getCurrentFragment(mScreenId);
        if (fragment != null && mStackManager.getCurrentFragment(mScreenId).getClass().getName().
                contains("NaviGuidanceFragment")) {
            bundle = new Bundle();
            bundle.putInt("bundle_key_route_start_navi_sim", 0);
        }
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            if (bundle != null) {
                onMoveMapCenter(bundle);
                return;
            }
            onMoveMapCenter();
        }
    }

    @Override
    public void closeAllFragment() {
        FragmentIntent.closeAllFragment(mScreenId, getSupportFragmentManager());
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
    }

    @Override
    public void closeAllFragmentUpRoute() {
        FragmentIntent.closeAllFragmentUpRoute(mScreenId, getSupportFragmentManager());
    }

    @Override
    public void closeAllFragmentsUntilTargetFragment(final String targetClassName) {
        FragmentIntent.closeAllFragmentsUntilTargetFragment(mScreenId, getSupportFragmentManager(), targetClassName);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
    }

    @Override
    public void closeAllFragmentAndSearchView() {
        FragmentIntent.closeAllFragment(mScreenId, getSupportFragmentManager());
        onMoveMapCenter();
    }

    @Override
    public void showCurrentFragment() {
        FragmentIntent.showCurrentFragment(mScreenId, getSupportFragmentManager());
    }

    protected VM initViewModel() {
        return null;
    }

    /**
     * 创建ViewModel
     */
    private void createViewModel() {
        mBinding = DataBindingUtil.setContentView(this, onLayoutId());
        final int mViewModelId = onInitVariableId();
        mViewModel = initViewModel();
        if (mViewModel == null) {
            final Class modelClass;
            final Type type = getClass().getGenericSuperclass();
            if (type instanceof ParameterizedType) {
                // 获取直接继承的父类(也就是BaseActivity本身)的第二个泛型参数Class
                modelClass = (Class) ((ParameterizedType) type).getActualTypeArguments()[1];
            } else {
                // If no generic parameters are specified, BaseViewModel is used by default.
                modelClass = BaseViewModel.class;
            }
            mViewModel = (VM) createViewModel(modelClass);
        }
        if (ConvertUtils.isEmpty(mViewModel)) {
            return;
        }
        mBinding.setVariable(mViewModelId, mViewModel);
        mBinding.setLifecycleOwner(this);
        getLifecycle().addObserver(mViewModel);
        mViewModel.attachView(this, mScreenId);
    }

    /**
     * 创建ViewModel
     *
     * @param cls ViewModel类
     * @param <T> AndroidViewModel
     * @return AndroidViewModel
     */

    private <T extends AndroidViewModel> T createViewModel(final Class<T> cls) {
        return new ViewModelProvider(this).get(cls);
    }

    protected void onMoveMapCenter() {

    }

    protected void onMoveMapCenter(final Bundle bundle) {

    }

    protected void onResetMapCenter() {

    }


    /**
     * 设置沉浸式状态栏
     */
    private void setImmersiveStatusBar() {
        final Window window = getWindow();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            // 清除之前的标志，确保没有残留影响
            window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            // 使内容布局延伸到状态栏下面
            window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN |
                    View.SYSTEM_UI_FLAG_LAYOUT_STABLE);
            // 允许设置状态栏的背景颜色
            window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            // 设置状态栏的颜色
            window.setStatusBarColor(0);
        } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            // 对于 Android 4.4 到 5.0 的系统，使用透明状态栏
            window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            // 调整布局，使内容布局延伸到状态栏下
            final View decorView = window.getDecorView();
            final int option = View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN | View.SYSTEM_UI_FLAG_LAYOUT_STABLE;
            decorView.setSystemUiVisibility(option);
        }
    }
}