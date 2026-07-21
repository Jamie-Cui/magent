from magent_benchmark.harbor_agent import ledger_to_trajectory


def test_ledger_converts_to_atif_with_tools_and_usage() -> None:
    ledger = {
        "id": "thread-1",
        "session-id": "session-1",
        "turns": [
            {
                "input": "Fix it",
                "items": [
                    {
                        "id": "user-1",
                        "type": "message",
                        "role": "user",
                        "content": "Fix it",
                        "created-at": 1_700_000_000.0,
                    },
                    {
                        "id": "assistant-1",
                        "type": "message",
                        "role": "assistant",
                        "content": "Done",
                        "completed-at": 1_700_000_003.0,
                    },
                    {
                        "id": "tool-item",
                        "type": "tool",
                        "name": "read_file",
                        "call-id": "call-1",
                        "input": {"path": "a.py"},
                        "output": "contents",
                        "status": "completed",
                        "phase": "completed",
                        "created-at": 1_700_000_001.0,
                    },
                ],
            }
        ],
    }
    result = {
        "provider": "openai",
        "wire-api": "responses",
        "usage-samples": [
            {"input_tokens": 100, "output_tokens": 20, "cached_tokens": 40},
            {"input_tokens": 120, "output_tokens": 10},
        ],
    }

    trajectory = ledger_to_trajectory(
        ledger,
        result,
        model_name="model-x",
        version="source-abc",
        input_price_per_million=2.0,
        output_price_per_million=10.0,
        cached_input_price_per_million=0.5,
    )
    assert trajectory.schema_version == "ATIF-v1.7"
    assert [step.source for step in trajectory.steps] == ["user", "agent", "agent"]
    assert trajectory.steps[1].tool_calls[0].function_name == "read_file"
    assert trajectory.steps[1].observation.results[0].source_call_id == "call-1"
    assert trajectory.steps[2].message == "Done"
    assert trajectory.final_metrics.total_prompt_tokens == 220
    assert trajectory.final_metrics.total_completion_tokens == 30
    assert trajectory.final_metrics.total_cached_tokens == 40
    assert trajectory.final_metrics.total_cost_usd == 0.00068
