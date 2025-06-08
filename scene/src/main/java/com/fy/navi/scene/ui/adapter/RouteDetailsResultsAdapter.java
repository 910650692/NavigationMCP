package com.fy.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteDetailsInfoResultParentItemBinding;
import com.fy.navi.scene.impl.route.common.SceneRouteCommonStruct;
import com.fy.navi.scene.impl.route.common.SceneRouteDetailEnumRes;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

public class RouteDetailsResultsAdapter extends RecyclerView.Adapter<RouteDetailsResultsAdapter.Holder> {
    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos; //
    private boolean mIsAvoid;
    private Hashtable<Integer, Boolean> mHashtable;
    private Hashtable<Integer, Boolean> mHashtableChild;
    private RouteDetailsResultsAdapter.OnItemClickListener mListener;

    public RouteDetailsResultsAdapter() {
        mRouteLineSegmentInfos = new ArrayList<>();
        mIsAvoid = false;
        mHashtable = new Hashtable<>();
        mHashtableChild = new Hashtable<>();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setAdapterResult(List<RouteLineSegmentInfo> routeLineSegmentInfos, boolean isAvoid) {
        this.mRouteLineSegmentInfos = routeLineSegmentInfos;
        this.mIsAvoid = isAvoid;
        for (int i = 0; i < routeLineSegmentInfos.size(); i++) {
            mHashtable.put(i, false);
            mHashtableChild.put(i, false);
        }
        notifyDataSetChanged();
    }

    /***
     * 设置监听
     * @param itemClickListener 点击监听
     */
    public void setItemClickListener(final RouteDetailsResultsAdapter.OnItemClickListener itemClickListener) {
        this.mListener = itemClickListener;
    }

    @Override
    public RouteDetailsResultsAdapter.Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteDetailsInfoResultParentItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_details_info_result_parent_item, parent, false);
        return new RouteDetailsResultsAdapter.Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mRouteLineSegmentInfos == null) {
            return 0;
        }
        return mRouteLineSegmentInfos.size();
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull final RouteDetailsResultsAdapter.Holder holder, final int position) {
        setFrontIcon(holder, position);
        holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemRoadName.setText(mRouteLineSegmentInfos.get(position).getMLoadName());
        holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemDescription.setText(mRouteLineSegmentInfos.get(position).getMDistance() + " "
                + AppCache.getInstance().getMContext().getResources().getString(R.string.route_details_light_count)
                + mRouteLineSegmentInfos.get(position).getMLightCount()
                + AppCache.getInstance().getMContext().getResources().getString(R.string.route_details_light_count_unit));
        Boolean isShowChild = mHashtableChild.get(position);
        holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemImgUpdown.setImageResource(Boolean.TRUE.equals(isShowChild) ? R.drawable.img_route_up : R.drawable.img_route_down);
        holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemRecyclerView.setVisibility(Boolean.TRUE.equals(isShowChild) ? View.VISIBLE : View.GONE);
        if (Boolean.TRUE.equals(isShowChild)) {
            final LinearLayoutManager layoutManager = new LinearLayoutManager(AppCache.getInstance().getMApplication());
            layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemRecyclerView.setLayoutManager(layoutManager);
            RouteDetailsResultsChildAdapter adapter = new RouteDetailsResultsChildAdapter();
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemRecyclerView.setAdapter(adapter);
            adapter.setAdapterResult(mRouteLineSegmentInfos.get(position).getMRouteLineSegmentInfos());
        }
        holder.mRouteDetailsInfoResultParentItemBinding.itemRootView.setOnClickListener(view -> {
            if (mIsAvoid) {
                if (!ConvertUtils.isEmpty(mListener)) {
                    mListener.onItemCheck(getRouteAvoidInfo(position, Boolean.FALSE.equals(mHashtable.get(position))));
                }
            } else {
                mHashtableChild.put(position, Boolean.FALSE.equals(isShowChild));
            }
            notifyItemChanged(position);
        });
    }

    /**
     * 初始化列表
     * @param holder view
     * @param groupPosition 列表索引
     * */
    private void setFrontIcon(final RouteDetailsResultsAdapter.Holder holder, final int groupPosition) {
        if (mIsAvoid) {
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemImg.setVisibility(View.GONE);
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemCbx.setVisibility(View.VISIBLE);
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemCbx.setChecked(Boolean.TRUE.equals(mHashtable.get(groupPosition)));

            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemCbx.setOnClickListener(view -> {
                if (ConvertUtils.isEmpty(mListener)) {
                    return;
                }
                mListener.onItemCheck(getRouteAvoidInfo(groupPosition, Boolean.FALSE.equals(mHashtable.get(groupPosition))));
            });
        } else {
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemCbx.setVisibility(View.GONE);
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemImg.setVisibility(View.VISIBLE);
            holder.mRouteDetailsInfoResultParentItemBinding.routeDetailInfoItemImg.setImageResource(SceneRouteDetailEnumRes.getDrawableEnumName(
                    SceneRouteCommonStruct.RouteDetailsMainAction.get(mRouteLineSegmentInfos.get(groupPosition).getMIconType())).getDayDrawableId());
        }
    }

    /**
     * 获取避开道路数据
     * @param groupPosition 索引
     * @param isChecked 是否勾选
     * @return 道路数据
     * */
    private RouteAvoidInfo getRouteAvoidInfo(final int groupPosition, final boolean isChecked) {
        mHashtable.put(groupPosition, isChecked);
        final RouteAvoidInfo info = new RouteAvoidInfo();
        boolean checkedLeastOne = false;
        final ArrayList<Long> avoidList = new ArrayList<>();
        for (boolean isCheck : mHashtable.values()) {
            checkedLeastOne |= isCheck;
            if (isCheck) {
                avoidList.addAll(mRouteLineSegmentInfos.get(groupPosition).getMAvoidList());
            }
        }
        info.setMCheckedLeastOne(checkedLeastOne);
        info.setMAvoidList(avoidList);
        return info;
    }

    public class Holder extends RecyclerView.ViewHolder {
        private final RouteDetailsInfoResultParentItemBinding mRouteDetailsInfoResultParentItemBinding;

        public Holder(final RouteDetailsInfoResultParentItemBinding routeDetailsInfoResultParentItemBinding) {
            super(routeDetailsInfoResultParentItemBinding.getRoot());
            this.mRouteDetailsInfoResultParentItemBinding = routeDetailsInfoResultParentItemBinding;
            routeDetailsInfoResultParentItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /***
         * 点击监听
         * @param index
         */
        void onItemClick(int index);
        /***
         * 点击监听
         * @param routeAvoidInfo
         */
        void onItemCheck(RouteAvoidInfo routeAvoidInfo);
    }
}
