package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteChildPoiItemBinding;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.view.SkinTextView;
import com.sgm.navi.ui.view.SkinView;

import java.util.ArrayList;
import java.util.List;

public class RouteChildPoiAdapter extends RecyclerView.Adapter<RouteChildPoiAdapter.Holder>{
    private PoiInfoEntity mPoiInfoEntity;
    private List<ChildInfo> mChildInfoList;
    private OnItemClickListener mItemClickListener;
    private int mSelected = -1;

    public RouteChildPoiAdapter() {
        mChildInfoList = new ArrayList<>();
    }

    /***
     * 设置子节点
     * @param childInfoList 子节点列表
     */
    public void setChildInfoList(final List<ChildInfo> childInfoList, final PoiInfoEntity poiInfoEntity) {
        if (childInfoList == null) {
            return;
        }
        mChildInfoList.clear();
        mChildInfoList.addAll(childInfoList);
        mPoiInfoEntity = poiInfoEntity;
        notifyDataSetChanged();
    }

    public void setSelected(final int selected) {
        this.mSelected = selected;
        if (mItemClickListener == null) {
            return;
        }
        if (mChildInfoList == null || mChildInfoList.isEmpty() || selected == -1 || selected >= mChildInfoList.size()) {
            return;
        }
        mItemClickListener.onItemClick(mChildInfoList.get(selected), mPoiInfoEntity, mSelected);
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteChildPoiItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_child_poi_item, parent, false);
        return new RouteChildPoiAdapter.Holder(routeItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        if (mSelected == position) {
            holder.mView.setVisibility(View.VISIBLE);
            holder.mTextView.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.text_color_route_item_select));
        } else {
            holder.mView.setVisibility(View.GONE);
            holder.mTextView.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.text_color_route_item_no_select));
        }
        holder.mTextView.setText(mChildInfoList.get(position).getShortName());
        holder.mTextView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                final int lastSelect = mSelected;
                setSelected(position);
                notifyItemChanged(lastSelect);
                notifyItemChanged(mSelected);

            }
        });
    }

    @Override
    public int getItemCount() {
        return mChildInfoList.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteChildPoiItemBinding mRouteChildPoiItemBinding;
        private SkinTextView mTextView;
        private SkinView mView;

        public Holder(@NonNull final RouteChildPoiItemBinding routeChildPoiItemBinding) {
            super(routeChildPoiItemBinding.getRoot());
            mRouteChildPoiItemBinding = routeChildPoiItemBinding;
            mTextView = mRouteChildPoiItemBinding.tvRouteChildPoi;
            mView = mRouteChildPoiItemBinding.routeItemSelected;
        }
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param childInfo 子节点对象
         * @param poiInfoEntity 当前位置
         * @param index 选中下标
         */
        void onItemClick(ChildInfo childInfo, PoiInfoEntity poiInfoEntity, int index);

    }
}
