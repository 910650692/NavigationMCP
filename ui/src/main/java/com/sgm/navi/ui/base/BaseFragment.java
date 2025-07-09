package com.sgm.navi.ui.base;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Objects;

public abstract class BaseFragment<V extends ViewDataBinding, VM extends BaseViewModel>
        extends Fragment implements IBaseView {
    protected V mBinding;
    protected VM mViewModel;
    protected BaseActivity mActivity;
    protected String mScreenId;

    private static final String KEY_CHANGE_SAVE_INSTANCE = "key_change_save_instance";
    private final String LIFE_CYCLE_TAG = "fragment_life_cycle";

    public BaseFragment() {
        Logger.i(getClass().getSimpleName(), "onCreate before");
        onCreateBefore();
    }

    @Override
    public void onAttach(final @NonNull Context context) {
        super.onAttach(context);
        mActivity = (BaseActivity) context;
        mScreenId = mActivity.mScreenId;
    }

    @Override
    public void onCreate(final @Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Logger.i(getClass().getSimpleName(), "onCreate start");
        createViewModel();
        Logger.i(getClass().getSimpleName(), "onCreate end");
    }

    @Nullable
    @Override
    public View onCreateView(final @NonNull LayoutInflater inflater, final @Nullable ViewGroup container, final @Nullable Bundle savedInstanceState) {
        Logger.i(getClass().getSimpleName(), "onCreateView start");
        mBinding = DataBindingUtil.inflate(inflater, onLayoutId(), container, false);
        bindViewModel();
        onInitView();
        final View rootView = mBinding.getRoot();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onCreateView end");
        return rootView;
    }

    @Override
    public void onViewCreated(final @NonNull View view, final @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        Logger.i(getClass().getSimpleName(), "onViewCreated start");
        onInitObserver();
        onInitData();
        if (ConvertUtils.isEmpty(savedInstanceState)
                || Boolean.FALSE.equals(Objects.requireNonNull(savedInstanceState).getBoolean(KEY_CHANGE_SAVE_INSTANCE))) {
            //todo 请在此方法里面请求数据，并将数据保存
            onGetFragmentData();
        } else {
            //todo 请在此方法里面使用保存数据刷新UI
            onReStoreFragment();
        }
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onViewCreated end");
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putBoolean(KEY_CHANGE_SAVE_INSTANCE, true);
    }

    @Override
    public void onStart() {
        super.onStart();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onStart");
    }

    @Override
    public void onResume() {
        super.onResume();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onResume");
    }

    @Override
    public void onPause() {
        super.onPause();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onPause");
    }

    @Override
    public void onStop() {
        super.onStop();
        if (mActivity != null) {
            mActivity.setMLastClosedFragmentName(getClass().getSimpleName());
        }
        Logger.i("NaviApp_Search", "onStop", getClass().getSimpleName());
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onDestroyView");
    }

    @Override
    public void onDetach() {
        super.onDetach();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onDetach");
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onDestroy");
    }

    @Override
    public void addFragment(final BaseFragment fragment, final Bundle bundle) {
        if (mActivity == null) {
            return;
        }
        mActivity.addFragment(fragment, bundle);
    }

    @Override
    public void addFragment(BaseFragment fragment, Bundle bundle, boolean isHideCurFragment) {
        if (mActivity == null) {
            return;
        }
        mActivity.addFragment(fragment, bundle, isHideCurFragment);
    }

    @Override
    public void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle) {
        if (mActivity == null) {
            return;
        }
        mActivity.addPoiDetailsFragment(fragment, bundle);
    }

    @Override
    public void closeFragment(final boolean nextShow) {
        if (mActivity == null) {
            return;
        }
        mActivity.closeFragment(nextShow);
    }

    @Override
    public void closeFragment(Bundle bundle) {
        if (mActivity == null) {
            return;
        }
        mActivity.closeFragment(bundle);
    }

    @Override
    public void closeAllFragment() {
        if (mActivity == null) {
            return;
        }
        mActivity.closeAllFragment();
    }

    @Override
    public void closeAllFragmentUpRoute() {
        if (mActivity == null) {
            return;
        }
        mActivity.closeAllFragmentUpRoute();
    }

    @Override
    public void closeAllFragmentUpNavi() {
        if (mActivity == null) {
            return;
        }
        mActivity.closeAllFragmentUpNavi();
    }

    @Override
    public void closeAllFragmentsUntilTargetFragment(final String targetFragmentClassName) {
        if (mActivity == null) {
            return;
        }
        mActivity.closeAllFragmentsUntilTargetFragment(targetFragmentClassName);
    }

    @Override
    public void closeAllFragmentAndSearchView() {
    }

    @Override
    public void showCurrentFragment() {
        if (mActivity == null) {
            return;
        }
        mActivity.showCurrentFragment();
    }

    protected void onNewIntent(final Bundle bundle) {

    }


    protected VM initViewModel() {
        return null;
    }

    /**
     * 创建ViewModel
     */
    private void createViewModel() {
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
    }

    /**
     * 创建ViewModel
     *
     * @param cls ViewModel
     * @param <T> AndroidViewModel
     * @return AndroidViewModel
     */
    private <T extends AndroidViewModel> T createViewModel(final Class<T> cls) {
        // TODO: 2023/8/17 2.5.0的viewModule暂时先采用这种方式加载
        return new ViewModelProvider(this).get(cls);
    }

    /**
     * 绑定ViewModel
     */
    private void bindViewModel() {
        mBinding.setVariable(onInitVariableId(), mViewModel);
        getLifecycle().addObserver(mViewModel);
        mBinding.setLifecycleOwner(this);
        mViewModel.attachView(this, mScreenId);
    }

    public String getLastClosedFragmentName() {
        if (mActivity != null) {
            return mActivity.getLastClosedFragmentName();
        } else {
            return "";
        }
    }
}