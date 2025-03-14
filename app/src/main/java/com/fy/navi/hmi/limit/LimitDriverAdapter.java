package com.fy.navi.hmi.limit;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.service.define.aos.RestrictedAreaDetail;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: QiuYaWei
 * Date: 2025/2/7
 * Description: [在这里描述文件功能]
 */
public class LimitDriverAdapter extends RecyclerView.Adapter<LimitDriverAdapter.LimitDriverViewHolder> {
    private ArrayList<RestrictedAreaDetail> data = new ArrayList<>();
    private Context mContext;

    public LimitDriverAdapter(Context context, List<RestrictedAreaDetail> list) {
        this.mContext = context;
        this.data.clear();
        this.data.addAll(list);
    }

    public void setData(List<RestrictedAreaDetail> list) {
        this.data.clear();
        this.data.addAll(list);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitDriverViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_policy, parent, false);
        return new LimitDriverViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LimitDriverViewHolder holder, int position) {
        RestrictedAreaDetail bean = data.get(position);
        holder.tvTitle.setText(String.format(mContext.getString(R.string.limit_policy_format), position + 1));
        holder.tvState.setVisibility(bean.effect == 1 ? View.VISIBLE : View.INVISIBLE);
        holder.tvTime.setText(bean.time);
        holder.tvDesc.setText(bean.summary + "\n" + bean.desc);
    }

    @Override
    public int getItemCount() {
        return data.size();
    }

    public static class LimitDriverViewHolder extends RecyclerView.ViewHolder {
        AppCompatTextView tvTitle, tvState, tvTime, tvDesc;

        public LimitDriverViewHolder(@NonNull View itemView) {
            super(itemView);
            tvTitle = itemView.findViewById(R.id.tv_title);
            tvState = itemView.findViewById(R.id.tv_state);
            tvTime = itemView.findViewById(R.id.tv_time);
            tvDesc = itemView.findViewById(R.id.tv_desc);
        }
    }
}
